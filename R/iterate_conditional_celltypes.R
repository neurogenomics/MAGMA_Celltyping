#' iterate_conditional_celltypes 
#' 
#' Support function for 
#' \link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
#' 
#' @inheritParams calculate_conditional_celltype_associations
#' @inheritParams celltype_associations_pipeline
#' 
#' @keywords internal  
iterate_conditional_celltypes <- function(ctd,
                                          EnrichmentMode,
                                          signifCells2,
                                          annotLevel,
                                          controlledAnnotLevel,
                                          genesCovarFile,
                                          controlCovarFile,
                                          controlledCovarCols, 
                                          magmaPaths, 
                                          upstream_kb, 
                                          downstream_kb,
                                          version){
    allRes <- 0
    for (i in seq_len(length(signifCells2))) {
        controlFor <- signifCells2[i]
        if (EnrichmentMode == "Linear") {
            if (annotLevel != controlledAnnotLevel) {
                genesCovarData <- utils::read.table(
                    file = genesCovarFile,
                    stringsAsFactors = FALSE,
                    header = TRUE,
                    check.names = FALSE
                )
                genesCovarData2 <- merge(
                    x = genesCovarData,
                    y = controlledCovarCols[, c("entrez", controlFor)]
                )
                write.table(
                    x = genesCovarData2,
                    file = genesCovarFile,
                    quote = FALSE,
                    row.names = FALSE,
                    sep = "\t"
                )
            } 
            sumstatsPrefix2 <-
                sprintf(
                    "%s.level%s.%sUP.%sDOWN.Linear.ControlFor_%s",
                    magmaPaths$filePathPrefix,
                    annotLevel,
                    upstream_kb,
                    downstream_kb,
                    controlFor
                )
            magma_cmd <- sprintf(
                paste(
                    "magma",
                    "--gene-results '%s.genes.raw'",
                    "--gene-covar '%s'",
                    "--model direction=pos condition='%s'",
                    "--out '%s'"
                ),
                magmaPaths$filePathPrefix,
                genesCovarFile,
                controlFor,
                sumstatsPrefix2
            )
        } else {
            controlledCovarCols2 <- controlledCovarCols
            colnames(controlledCovarCols2)[-1] <-
                sprintf("%s.covar", colnames(controlledCovarCols2)[-1])
            write.table(
                x = controlledCovarCols2,
                file = controlCovarFile,
                quote = FALSE,
                row.names = FALSE,
                sep = "\t"
            )
            controlledCTcovarNames <- colnames(controlledCovarCols2)[-1]
            sumstatsPrefix2 <- sprintf(
                "%s.level%s.%sUP.%sDOWN.Top10.ControlFor_%s",
                magmaPaths$filePathPrefix,
                annotLevel,
                upstream_kb,
                downstream_kb,
                controlFor
            )
            # First match quantiles to the genes in the genes.out file...
            # then write as the genesCovar file (the input to MAGMA)
            magma_cmd <- sprintf(
                paste(
                    "magma",
                    "--gene-results '%s.genes.raw'",
                    "--set-annot '%s'",
                    "--gene-covar '%s'",
                    "--model direction=pos  condition=%s",
                    "--out '%s'"
                ),
                magmaPaths$filePathPrefix,
                genesCovarFile,
                controlCovarFile,
                sprintf("%s.covar", controlFor),
                sumstatsPrefix2
            )
        }
        #### Run MAGMA command ####
        magma_run(cmd = magma_cmd, 
                  version = version) 
        cond_res <- load_magma_results_file(
            path = sprintf("%s.gsa.out", sumstatsPrefix2), 
            annotLevel = annotLevel, 
            ctd = ctd, 
            genesOutCOND = NA, 
            EnrichmentMode = EnrichmentMode, 
            ControlForCT = controlFor) 
        if (i == 1) {
            allRes <- cond_res
        } else {
            allRes <- rbind(allRes, cond_res)
        }
    } #### End for loop
    return(allRes)
}
