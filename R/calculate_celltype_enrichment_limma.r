#' Calculate celltype enrichment
#'
#' Calculate celltype enrichments without MAGMA
#' using adjusted MAGMA Z-statistic from .genes.out files.
#'
#' @param magmaAdjZ Output from
#'  \link[MAGMA.Celltyping]{adjust_zstat_in_genesOut}.
#' @param thresh A threshold on low specificity values (used to drop genes).
#' @param annotLevel Level within the \code{ctd} to use.
#' @param celltypes Cell types to conduct tests for. Defaults to all cell types
#' available in selected \code{ctd} level.
#' @param return_all Return a list of both the processed 
#' data input and the enrichment results (Default).
#'  If \code{FALSE}, will only return enrichment results.
#' @param ... Additional arguments passed to \link[EWCE]{standardise_ctd}.
#' @inheritParams calculate_celltype_associations
#'
#' @examples
#' ctd <- ewceData::ctd()
#'
#' # The package stores an example genesOut file, so save this to a tempfile
#' myGenesOut <- MAGMA.Celltyping::import_magma_files(
#'     ids = c("ieu-a-298"),
#'     file_types = ".genes.out",
#'     return_dir = FALSE)
#'     
#' #### Get adjust gene z-scores ####    
#' magmaAdjZ <- MAGMA.Celltyping::adjust_zstat_in_genesOut(
#'     ctd = ctd,
#'     magma_GenesOut_file = myGenesOut)
#'     
#' #### Get cell type enrichment ####
#' ps <- MAGMA.Celltyping::calculate_celltype_enrichment_limma(
#'     magmaAdjZ = magmaAdjZ,
#'     ctd = ctd)
#' @export
#' @importFrom dplyr mutate rename filter
#' @importFrom methods is
#' @importFrom limma lmFit eBayes topTable
#' @importFrom stats quantile setNames 
#' @importFrom data.table rbindlist
#' @importFrom EWCE fix_celltype_names standardise_ctd
calculate_celltype_enrichment_limma <- function(
    magmaAdjZ,
    ctd, 
    ctd_species = infer_ctd_species(ctd),
    annotLevel = 1,
    prepare_ctd = TRUE,
    thresh = 0.0001,
    celltypes = NULL,
    return_all = FALSE,
    verbose = TRUE,
    ...) {
    
    #### Avoid confusing checks ####
    hgnc.symbol <- human.symbol <- percentile <- NULL; 
    Celltype_id <- NULL;
    
    #### Check hgnc_symbol are present ####
    if (!"hgnc_symbol" %in% colnames(magmaAdjZ)) {
        if("hgnc.symbol" %in% colnames(magmaAdjZ)){
            magmaAdjZ <- magmaAdjZ |>
                dplyr::rename(hgnc_symbol = hgnc.symbol)
        } else if ("human.symbol" %in% colnames(magmaAdjZ)){
            magmaAdjZ <- magmaAdjZ |>
                dplyr::rename(hgnc_symbol = human.symbol)
        }else {
            stopper("Could not find hgnc_symbol or hgnc.symbol column ",
                    "in magmaAdjZ.")
        } 
    }
    #### Standardise CTD ####
    og_colnames <- colnames(ctd[[annotLevel]]$specificity)
    if(prepare_ctd){
        output_species <- "human"
        ctd <- EWCE::standardise_ctd(ctd = ctd,
                                     dataset = "CTD",
                                     input_species = ctd_species,
                                     output_species = output_species,
                                     verbose = verbose,
                                     ...)
        ctd_species <- output_species
    }
    #### Get names of all cell types ####
    #### Ensure cell-type names are processed
    ## the same as in the MAGMA.Celltyping pipeline
    messager("Preparing specificity matrix.", v = verbose)
    spec <- ctd[[annotLevel]]$specificity 
    if(methods::is(spec,"data.frame")){
        spec <- as.matrix(spec)
    } 
    allCellTypes <- EWCE::fix_celltype_names(colnames(spec))
    celltype_dict <- stats::setNames(og_colnames, allCellTypes)
    #### Select a subset of cell types to test####
    if (!is.null(celltypes)) {
        celltypes <- EWCE::fix_celltype_names(celltypes = celltypes) 
        allCellTypes <- allCellTypes[allCellTypes %in% celltypes]
        messager(length(allCellTypes), "cell-types selected.", v = verbose)
    }
    #### Initialise variables ####
    ps <- coef <- rep(0, length(allCellTypes))
    names(ps) <- names(coef) <- allCellTypes
    #### Loop over each celltype testing for enrichment ####
    count <- 0
    res_all <- list()
    input_all <- list()
    for (ct1 in allCellTypes) {
        count <- count + 1
        messager("Running enrichment tests:",ct1,v=verbose)
        hgncS <- magmaAdjZ$hgnc_symbol
        props <- spec[hgncS, ct1]
        notExp <- rep(0, length(props))
        #### Drop any genes with expression below threshold ####
        hgncS <- hgncS[props > thresh]
        props <- props[props > thresh]
        notExp[props < thresh] <- -1
        #### Determine which expression decile genes fall into ####
        quantiles <- stats::quantile(props[props > thresh],
                                     probs = seq(from = 0, to = 1, by = 0.1))
        perc <- as.numeric(cut(props, quantiles, include.lowest = TRUE))
        perc[is.na(perc)] <- 0
        #### Merge decile groups with MAGMA zscores ####
        geneGroups <- data.frame(
            hgnc_symbol = hgncS,
            proportion = props,
            percentile = perc
        )
        magma_with_ct1 <- geneGroups |>
            merge(magmaAdjZ, by = "hgnc_symbol") |>
            dplyr::filter(percentile >= 0)
        input_all[[ct1]] <- magma_with_ct1
        #### Fit a linear model and get p-value and coefficient (slope) ####
        expMat <- matrix(0, nrow = 1, ncol = dim(magma_with_ct1)[1])
        expMat[1, ] <- magma_with_ct1$ADJ_ZSTAT
        rownames(expMat) <- c("actual")
        colnames(expMat) <- magma_with_ct1$hgnc_symbol
        dmat <- stats::model.matrix(~ magma_with_ct1$percentile)
        fit <- limma::lmFit(expMat, dmat)
        fit2 <- limma::eBayes(fit)
        res <- limma::topTable(fit2, number = 1)
        res[res$logFC < 0, "P.Value"] <- 1
        res[res$logFC >= 0, "P.Value"] <- res[res$logFC >= 0, "P.Value"] / 2
        #### Convert p-value to one-sided ####
        ps[count] <- res$P.Value
        res_all[[ct1]] <- res
    }

    if (return_all) {
        #### res df ####
        ## Add columns to differentiation original cell type names
        ## from standardized cell type names.
        res_all_df <- data.table::rbindlist(res_all, 
                                            use.names = TRUE,
                                            idcol = "Celltype_id") |>
            dplyr::mutate(Celltype = celltype_dict[Celltype_id],
                          ctd_level = annotLevel,
                          .after = 1) 
        #### input df ####
        input_all_df <- data.table::rbindlist(input_all,
                                              use.names = TRUE,
                                              idcol = "Celltype_id") |>
            dplyr::rename(
                specificity_proportion = dplyr::all_of("proportion"),
                specificity_decile = dplyr::all_of("percentile")) |>
            dplyr::mutate(Celltype = celltype_dict[Celltype_id],
                          ctd_level = annotLevel,
                          .after = 1) 

        return(list(
            input = input_all_df,
            results = res_all_df
        ))
    } else {
        return(ps)
    }
}

calculate.celltype.enrichment.probabilities.wtLimma <- function(...) {
    .Deprecated("calculate_celltype_enrichment_limma")
    calculate_celltype_enrichment_limma(...)
}
