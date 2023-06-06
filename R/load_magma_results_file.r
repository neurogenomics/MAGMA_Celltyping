#' Load a MAGMA .gcov.out file
#'
#' Convenience function which just does a little formatting to make 
#' it easier to use.
#'
#' @param path Path to a .gcov.out file (if \code{EnrichmentMode=='Linear'}).
#'  or .sets.out (if EnrichmentMode=='Top 10\%').
#' @param genesOutCOND [Optional] If the analysis controlled for another GWAS, 
#' then this is included as a column. Otherwise the column is \code{NA}.
#' @param EnrichmentMode [Optional] Either 'Linear' or 'Top 10\%' 
#' (Default: \code{'Linear'}).
#' @param ControlForCT [Optional] May be an internal argument. 
#' We suggest ignoring or take a look at the code to figure it out.
#' @param keep_all_rows Keep all rows, without filtering  
#' such that there is only one row per celltype.
#' @param verbose Print messages.
#' @inheritParams calculate_celltype_associations
#' @inheritParams calculate_celltype_enrichment_limma 
#' @returns Contents of the .gcov.out file.
#' 
#' @export
#' @importFrom utils read.table
#' @examples
#' path <- system.file(
#'     "extdata","ieu-a-298.tsv.gz.35UP.10DOWN.level1.MainRun.gsa.out",
#'     package = "MAGMA.Celltyping")
#' ctd <- ewceData::ctd()
#' annotLevel <- 1
#' EnrichmentMode <- "Linear"
#' res <- load_magma_results_file(path = path, 
#'                                ctd = ctd,
#'                                annotLevel = annotLevel,
#'                                EnrichmentMode = EnrichmentMode)
load_magma_results_file <- function(path,
                                    ctd,
                                    annotLevel,
                                    EnrichmentMode,
                                    genesOutCOND = NA,
                                    analysis_name = NULL,
                                    ControlForCT = "BASELINE",
                                    keep_all_rows = FALSE,
                                    verbose = TRUE) {
    # devoptera::args2vars(load_magma_results_file)
    requireNamespace("utils")
    requireNamespace("dplyr")
    VARIABLE <- NGENES <- TYPE <- NULL;
    
    messager("Reading enrichment results file into R.",v=verbose)
    force(path)
    force(ctd)
    force(annotLevel) 
    genesOutCOND <- genesOutCOND[1]
    path <- get_actual_path(path)
    #### Check EnrichmentMode has correct values ####
    check_enrichment_mode(EnrichmentMode = EnrichmentMode)
    #### Check the file has appropriate ending given EnrichmentMode ####
    if (EnrichmentMode == "Linear" &&
        length(grep(".gsa.out$|.gsa.out.txt$", path)) == 0) {
        stop("If EnrichmentMode=='Linear' then path must end in '.gsa.out'")
    }
    {
      res <- utils::read.table(path, stringsAsFactors = FALSE, header = FALSE)
      colnames(res) <- as.character(res[1, ])
      res <- res[-1, ]
    }
    # Check if some of the variables are ZSTAT
    # (if so, this indicates that another GWAS is being controlled for)
    isConditionedOnGWAS <- (sum(grepl("ZSTAT", colnames(res))) > 0 ) |
        sum(grepl("^ZSTAT", res$VARIABLE))>0
    # The VARIABLE column in MAGMA output is limited by 30 characters.
    # If so, use the FULL_NAME column.
    if (!is.null(res$FULL_NAME)) {
        res$VARIABLE <- res$FULL_NAME
    }
    #### Do some error checking ####
    ctd_celltypes <- colnames(ctd[[annotLevel]]$specificity)
    res_celltypes <- unique(
        grep("^ZSTAT",res$VARIABLE, value = TRUE, invert = TRUE)
    )
    numCTinCTD <- length(ctd_celltypes) 
    numCTinRes <- length(res_celltypes)
    if (ControlForCT[1] == "BASELINE" &&
        isFALSE(isConditionedOnGWAS)) {
        if (numCTinCTD != numCTinRes) {
            if (abs(numCTinCTD - numCTinRes) > as.integer(numCTinRes * .5)) {
                stop(sprintf(
                    ">50% of celltypes missing. %s celltypes
                             in ctd but %s in results file.
                             Did you provide the correct annotLevel?",
                    numCTinCTD, numCTinRes
                ))
            } else {
                messager(numCTinCTD,
                "celltypes in ctd but",numCTinRes,"in results file.",
                "Some celltypes may have been dropped due if the variance",
                "being too low (i.e. set contains only one gene used in analysis).")
                messager(
                    "<50% of celltypes missing.",
                    "Attemping to fix by removing missing cell-types:\n",
                    paste(" - ",
                          base::setdiff(
                              ctd_celltypes,
                              res_celltypes
                          ),
                          collapse = "\n"
                    )
                )
                res <- subset(res, 
                              VARIABLE %in% ctd_celltypes |
                                  startsWith(VARIABLE,"ZSTAT")) 
            } 
        } 
    }  
    # In the new version of MAGMA, when you run conditional analyses,
    # each model has a number, and the covariates also get p-values...
    # ---- The information contained is actually quite useful....
    # but for now just drop it 
    if(isFALSE(keep_all_rows)){
        if(EnrichmentMode=="Linear"){
            res <- subset(res, TYPE=="COVAR" & (VARIABLE %in% res_celltypes))    
        } else if (EnrichmentMode=="Top 10%"){
            res <- subset(res, TYPE=="SET" & (VARIABLE %in% res_celltypes))    
        }
    }  
    res$BETA <- as.numeric(res$BETA)
    res$BETA_STD <- as.numeric(res$BETA_STD)
    res$SE <- as.numeric(res$SE)
    res$P <- as.numeric(res$P)
    res$log10p <- log(res$P, 10)
    res$level <- annotLevel
    res$Method <- "MAGMA"
    res$EnrichmentMode <- EnrichmentMode 
    res$GCOV_FILE <- basename(path)
    res$CONTROL <- paste(ControlForCT, collapse = ",")
    res$CONTROL_label <- paste(ControlForCT, collapse = ",")
    res$genesOutCOND <- paste(genesOutCOND, collapse = " | ")
    res$analysis_name <- analysis_name
    res <- res |>
        dplyr::rename(Celltype = VARIABLE,
                      OBS_GENES = NGENES) 
    res$SET <- NULL 
    return(res)
}


load.magma.results.file <- function(...) {
    .Deprecated("load_magma_results_file")
    load_magma_results_file(...)
}
