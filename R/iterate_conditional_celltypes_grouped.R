#' iterate_conditional_celltypes: grouped
#' 
#' Support function for 
#' \link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
#' 
#' @param allRes Results from
#'  \code{MAGMA.Celltyping::iterate_conditional_celltypes}. 
#' @inheritParams calculate_conditional_celltype_associations
#' @inheritParams celltype_associations_pipeline
#' 
#' @keywords internal  
iterate_conditional_celltypes_grouped <- function(allRes,
                                                  ctd,
                                                  signifCells2,
                                                  EnrichmentMode, 
                                                  annotLevel,
                                                  controlledAnnotLevel,
                                                  genesCovarFile,
                                                  controlCovarFile,
                                                  controlledCovarCols,
                                                  magmaPaths,
                                                  upstream_kb, 
                                                  downstream_kb,
                                                  version,
                                                  verbose){
    if(length(signifCells2)==0){
        stopper("Must provide at least one celltype to control for.")
    }
    pastedControls <- paste(signifCells2, collapse = ",")
    if (EnrichmentMode == "Linear") {
        if (annotLevel != controlledAnnotLevel) {
            genesCovarData <- utils::read.table(
                file = genesCovarFile, 
                stringsAsFactors = FALSE, 
                header = TRUE)
            genesCovarData2 <- merge(
                x = genesCovarData,
                y = controlledCovarCols[, c("entrez", signifCells2)])
            write.table(x = genesCovarData2,
                        file = genesCovarFile, 
                        quote = FALSE, 
                        row.names = FALSE,
                        sep = "\t")
        }
        sumstatsPrefix2 <- sprintf("%s.level%s.%sUP.%sDOWN.ControlFor_%s",
                                   magmaPaths$filePathPrefix, 
                                   annotLevel, 
                                   upstream_kb, 
                                   downstream_kb, 
                                   pastedControls)
        magma_cmd <- sprintf(
            paste("magma",
                  "--gene-results '%s.genes.raw'",
                  "--gene-covar '%s'",
                  "--model direction=pos condition='%s'",
                  "--out '%s'"), 
            magmaPaths$filePathPrefix, 
            genesCovarFile, 
            pastedControls, 
            sumstatsPrefix2)
    } else {
        controlledCovarCols2 <- controlledCovarCols
        colnames(controlledCovarCols2)[-1] <- sprintf(
            "%s.covar", colnames(controlledCovarCols2)[-1])
        utils::write.table(x = controlledCovarCols2, 
                    file = controlCovarFile, 
                    quote = FALSE, 
                    row.names = FALSE, 
                    sep = "\t")
        controlledCTcovarNames <- colnames(controlledCovarCols2)[-1]
        pastedControlCovars <- paste(controlledCTcovarNames,
                                     collapse = ",")
        sumstatsPrefix2 <- sprintf(
            "%s.level%s.%sUP.%sDOWN.Top10.ControlFor_%s",
            magmaPaths$filePathPrefix,
            annotLevel, 
            upstream_kb, 
            downstream_kb,
            pastedControls)
        magma_cmd <- sprintf(
            paste("magma",
                  "--gene-results '%s.genes.raw'",
                  "--set-annot '%s'",
                  "--gene-covar '%s'",
                  "--model direction=pos condition=%s",
                  "--out '%s'"),
            magmaPaths$filePathPrefix,
            genesCovarFile, 
            controlCovarFile, 
            pastedControlCovars, 
            sumstatsPrefix2)
    }
    magma_run(cmd = magma_cmd, 
              version = version,
              verbose = verbose)
    
    cond_res <- load_magma_results_file(path = sprintf("%s.gsa.out",
                                                       sumstatsPrefix2),
                                        annotLevel =  annotLevel,
                                        ctd = ctd, 
                                        genesOutCOND = NA, 
                                        EnrichmentMode = EnrichmentMode,
                                        ControlForCT = pastedControls, 
                                        verbose = verbose)
    allRes <- rbind(allRes, cond_res)
    return(allRes)
}
