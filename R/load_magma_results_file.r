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
#' @inheritParams calculate_celltype_associations
#' @inheritParams calculate_celltype_enrichment_limma
#' @return Contents of the .gcov.out file.
#' 
#' @keywords internal 
load_magma_results_file <- function(path,
                                    annotLevel,
                                    ctd,
                                    genesOutCOND = NA,
                                    EnrichmentMode = "Linear",
                                    ControlForCT = "BASELINE") {
    requireNamespace("utils")
    requireNamespace("dplyr")
    #### Avoid confusing checks ####
    VARIABLE <- NGENES <- NULL;
    #### Check EnrichmentMode has correct values ####
    check_enrichment_mode(EnrichmentMode = EnrichmentMode)
    #### Check the file has appropriate ending given EnrichmentMode ####
    if (EnrichmentMode == "Linear" && length(grep(".gsa.out$", path)) == 0) {
        stop("If EnrichmentMode=='Linear' then path must end in '.gsa.out'")
    }
    res <- utils::read.table(path, stringsAsFactors = FALSE)
    colnames(res) <- as.character(res[1, ])
    res$level <- annotLevel
    res <- res[-1, ]

    # Check if some of the variables are ZSTAT
    # (if so, this indicates that another GWAS is being controlled for)
    isConditionedOnGWAS <- sum(grepl("ZSTAT", colnames(res))) > 0

    # The VARIABLE column in MAGMA output is limited by 30 characters.
    # If so, use the FULL_NAME column.
    if (!is.null(res$FULL_NAME)) {
        res$VARIABLE <- res$FULL_NAME
    }

    # Do some error checking
    numCTinCTD <- length(colnames(ctd[[annotLevel]]$specificity))
    numCTinRes <- dim(res)[1]
    if (ControlForCT[1] == "BASELINE" & !isConditionedOnGWAS) {
        if (numCTinCTD != numCTinRes) {
            if (abs(numCTinCTD - numCTinRes) > as.integer(numCTinRes * .5)) {
                stop(sprintf(
                    ">50% of celltypes missing. %s celltypes
                             in ctd but %s in results file.
                             Did you provide the correct annotLevel?",
                    numCTinCTD, numCTinRes
                ))
            }
            messager(sprintf("%s celltypes in ctd but %s in results file.
                      Some cell-types may have been dropped due to
                             'variance is too low (set contains only one
                             gene used in analysis)'.", numCTinCTD, numCTinRes))
            messager(
                "<50% of celltypes missing.",
                "Attemping to fix by removing missing cell-types:\n",
                paste(" - ",
                    dplyr::setdiff(
                        colnames(ctd[[annotLevel]]$specificity),
                        res$VARIABLE
                    ),
                    collapse = "\n"
                )
            )
            ctd_colnames <- colnames(ctd[[annotLevel]]$specificity)
            res <- subset(res, VARIABLE %in% ctd_colnames)
            res$COVAR <- ctd_colnames[ctd_colnames %in% res$VARIABLE]
        } else {
            res$COVAR <- colnames(ctd[[annotLevel]]$specificity)
        }
    } else {
        tmpF <- tempfile()
        tmpDat <- t(data.frame(
            original = colnames(ctd[[annotLevel]]$specificity),
            stringsAsFactors = FALSE,
            check.names = FALSE,
            check.rows = FALSE
        ))
        colnames(tmpDat) <- colnames(ctd[[annotLevel]]$specificity)
        utils::write.csv(tmpDat, file = tmpF)
        editedNames <- colnames(utils::read.csv(
            file = tmpF,
            stringsAsFactors = FALSE
        ))[-1]
        transliterateMap <- data.frame(
            original = colnames(ctd[[annotLevel]]$specificity),
            edited = editedNames,
            stringsAsFactors = FALSE,
            check.rows = FALSE,
            check.names = FALSE
        )
        rownames(transliterateMap) <- transliterateMap$edited
        # res = res[res$COVAR %in% rownames(transliterateMap),]
        res <- res[res$VARIABLE %in% rownames(transliterateMap), ]
        # res$COVAR = transliterateMap[res$COVAR,]$original
        res$VARIABLE <- transliterateMap[res$VARIABLE, ]$original
    }

    # In the new version of MAGMA, when you run conditional analyses,
    # each model has a number, and the covariates also get p-values...
    # ---- The information contained is actually quite useful....
    # but for now just drop it
    if (sum(res$MODEL == 1) > 1) {
        xx <- sort(table(res$VARIABLE), decreasing = TRUE)
        tt <- xx[xx > 1]
        controlledCTs <- names(tt)
        res <- res[!res$VARIABLE %in% controlledCTs, ]
    }

    # rownames(res) = res$COVAR
    rownames(res) <- res$VARIABLE
    res$BETA <- as.numeric(res$BETA)
    res$BETA_STD <- as.numeric(res$BETA_STD)
    res$SE <- as.numeric(res$SE)
    res$P <- as.numeric(res$P)
    res$Method <- "MAGMA"
    res$GCOV_FILE <- basename(path)
    res$CONTROL <- paste(ControlForCT, collapse = ",")
    res$CONTROL_label <- paste(ControlForCT, collapse = ",")
    res$log10p <- log(res$P, 10)
    res$genesOutCOND <- paste(genesOutCOND, collapse = " | ")
    res$EnrichmentMode <- EnrichmentMode 
    res <- res %>%
        dplyr::rename(Celltype = VARIABLE,
                      OBS_GENES = NGENES) #%>%
        # purrr::modify_at(c("SET"), ~NULL)
    res$SET <- NULL
    res <- res[, c(
        "Celltype", "OBS_GENES", "BETA", "BETA_STD",
        "SE", "P", "level", "Method", "GCOV_FILE",
        "CONTROL", "CONTROL_label", "log10p",
        "genesOutCOND", "EnrichmentMode"
    )]
    # }

    return(res)
}


load.magma.results.file <- function(...) {
    .Deprecated("load_magma_results_file")
    load_magma_results_file(...)
}
