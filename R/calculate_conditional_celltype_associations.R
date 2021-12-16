#' Calculate conditional celltype associations using MAGMA
#'
#' Assumes that you have already run \link[MAGMA.Celltyping]{map_snps_to_genes}.
#'
#' @param controlledAnnotLevel Which annotation level should be controlled for.
#' @param controlTopNcells How many of the most significant cell types at
#' that annotation level should be controlled for?
#' @param controlledCTs Array of the celltype to be controlled for,
#' e.g. \code{c('Interneuron type 16','Medium Spiny Neuron')}. 
#' @inheritParams celltype_associations_pipeline
#' @inheritParams calculate_celltype_associations
#'
#' @returns Conditional enrichment results,
#'  or \code{NULL} is no results were significant. 
#'
#' @examples
#' #### Prepare cell-type data ####
#' ctd <- ewceData::ctd()
#' 
#' #### Prepare GWAS MAGMA data ####
#' magma_dir <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298")
#'     
#' #### Run pipeline ####
#' ctAssocs <- MAGMA.Celltyping::calculate_conditional_celltype_associations(
#'     ctd = ctd,
#'     controlledAnnotLevel = 1,
#'     controlTopNcells = 1,
#'     magma_dir = magma_dir,
#'     ctd_species = "mouse") 
#' @export
#' @importFrom utils read.table
calculate_conditional_celltype_associations <- function(
    ctd,
    ctd_species = "mouse",
    gwas_sumstats_path = NULL,
    magma_dir = NULL,
    analysis_name = "MainRun",
    prepare_ctd = TRUE,
    upstream_kb = 35,
    downstream_kb = 10, 
    controlledAnnotLevel = 1,
    controlTopNcells = NA,
    controlledCTs = NA,
    EnrichmentMode = "Linear",
    force_new = FALSE,
    version = NULL,
    verbose = TRUE) {
    
    #### Check MAGMA installation ####
    magma_check(version = version,
                verbose = verbose)
    #### Check args ####
    check_enrichment_mode(EnrichmentMode = EnrichmentMode)
    #### Handle MAGMA Files ####
    #### Trick downstream functions into working with only MAGMA files ####
    magma_dir <- magma_dir[1]
    if(!is.null(magma_dir)){ 
        gwas_sumstats_path <- create_fake_gwas_path(
            magma_dir = magma_dir,
            upstream_kb = upstream_kb,
            downstream_kb = downstream_kb)
    }
    #### prepare quantile groups ####
    # MAGMA.Celltyping can only use human GWAS
    if (prepare_ctd) {
        output_species <- "human"
        ctd <- prepare_quantile_groups(
            ctd = ctd,
            input_species = ctd_species,
            output_species = output_species,
            verbose = verbose
        )
        ctd_species <- output_species
    } 
    #### Setup paths ####
    gwas_sumstats_path <- path.expand(gwas_sumstats_path)
    magmaPaths <- get_magma_paths(
        gwas_sumstats_path = gwas_sumstats_path,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )
    #### Check for errors in arguments ####
    check_inputs_to_magma_celltype_analysis(
        ctd = ctd,
        gwas_sumstats_path = gwas_sumstats_path, 
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )
    #### Either controlTopNcells or controlledCTs should be passed ####
    # ...not both
    if (!is.na(controlTopNcells) && !is.na(controlledCTs)) {
        stopper(
            "Either controlTopNcells or controlledCTs",
            "should be passed with arguments, not both."
        )
    }
    #### If both are NA then also reject that ####
    if (is.na(controlTopNcells) && is.na(controlledCTs)) {
        stopper(
            "Either controlTopNcells or controlledCTs",
            "should be passed with arguments."
        )
    }
    ##### Calculate the baseline associations ####
    ctAssocs <- calculate_celltype_associations(
        ctd = ctd,
        ### Only uses the level specified by controlledAnnotLevel
        ### Running all levels is a wasteful computation here.
        ctd_levels = controlledAnnotLevel,
        analysis_name = analysis_name,
        prepare_ctd = prepare_ctd,
        gwas_sumstats_path = gwas_sumstats_path, 
        ctd_species = ctd_species,
        EnrichmentMode = EnrichmentMode,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb,
        force_new = force_new,
        verbose = verbose
    )

    if (!is.na(controlledCTs[1])) {
        # Check if controlledCTs are all in the CTD
        # at the expected annotation level
        if (mean(controlledCTs %in% colnames(
            ctd[[controlledAnnotLevel]]$specificity
        )) < 1) {
            missingCTs <- controlledCTs[
                !controlledCTs %in% colnames(
                    ctd[[controlledAnnotLevel]]$specificity
                )
            ]
            stopper(
                "The following celltypes are not found at",
                "the specified annotation level:",
                paste(missingCTs, sep = " ")
            )
        } else {
            signifCells <- controlledCTs
        }
    } else {
        # Find the cells which are most significant at baseline
        # at controlled annotation level
        res <- ctAssocs[[controlledAnnotLevel]]$results
        res <- res[order(res$P), ]
        signifCells <- as.character(
            res[
                res$P < (0.05 / ctAssocs$total_baseline_tests_performed),
                "Celltype"
            ]
        )

        if (length(signifCells) > controlTopNcells) {
            signifCells <- signifCells[seq_len(controlTopNcells)]
        }

        # If there are no significant cells... then stop
        if (length(signifCells) == 0) {
            messager("Warning: No annotLevel",controlledAnnotLevel,
                     "celltypes reach significance with Q<0.05:",
                     "returning NULL.")
            return(NULL)
        }
    }

    # Create gene covar file for the controlled for annotation level
    controlledCovarFile <- create_gene_covar_file(
        genesOutFile = sprintf("%s.genes.out", magmaPaths$filePathPrefix),
        ctd = ctd,
        annotLevel = controlledAnnotLevel,
        ctd_species = ctd_species
    )
    # Read in the controlled Covar File
    controlledCovarData <- utils::read.table(
        file = controlledCovarFile,
        stringsAsFactors = FALSE,
        header = TRUE,
        check.names = FALSE
    )
    transliterateMap <- data.frame(
        original = colnames(ctd[[
        controlledAnnotLevel]]$specificity_quantiles),
        modified = colnames(controlledCovarData)[
            seq(2, length(colnames(controlledCovarData)))
        ],
        stringsAsFactors = FALSE,
        check.names = FALSE,
        check.rows = FALSE
    )
    if (!is.na(controlledCTs[1])) {
        # Because full stops replace spaces when the covars
        # are written to file... (and MAGMA sees spaces as delimiters)
        signifCells2 <- transliterateMap[
            transliterateMap$original %in% signifCells,
        ]$modified
    } else {
        signifCells2 <- signifCells
    }
    controlledCovarCols <- controlledCovarData[, c("entrez", signifCells2)]
    controlCovarFile <- tempfile()
    write.table(
        x = controlledCovarCols,
        file = controlCovarFile,
        quote = FALSE,
        row.names = FALSE,
        sep = "\t"
    )

    for (annotLevel in seq_len(length(ctd))) {
        count <- allRes <- 0
        # First match quantiles to the genes in the genes.out file...
        # then write as the genesCovar file (the input to MAGMA)
        if (EnrichmentMode == "Linear") {
            genesCovarFile <- create_gene_covar_file(
                genesOutFile = sprintf(
                    "%s.genes.out", magmaPaths$filePathPrefix
                ),
                ctd = ctd,
                annotLevel = annotLevel,
                ctd_species = ctd_species
            )
        } else {
            geneCovarFile <- create_top10percent_genesets_file(
                genesOutFile = sprintf(
                    "%s.genes.out", magmaPaths$filePathPrefix
                ),
                ctd = ctd,
                annotLevel = annotLevel,
                ctd_species = ctd_species
            )
        }

        # First control for each individually
        for (controlFor in signifCells2) {
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
                    ), magmaPaths$filePathPrefix,
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
                    geneCovarFile,
                    controlCovarFile,
                    sprintf("%s.covar", controlFor), sumstatsPrefix2
                )
            }
            message(magma_cmd)
            system(magma_cmd)

            cond_res <- load_magma_results_file(
                path = sprintf("%s.gsa.out", sumstatsPrefix2), 
                annotLevel = annotLevel, 
                ctd = ctd, 
                genesOutCOND = NA, 
                EnrichmentMode = EnrichmentMode, 
                ControlForCT = controlFor)
            count <- count + 1
            if (count == 1) {
                allRes <- cond_res
            } else {
                allRes <- rbind(allRes, cond_res)
            }
        }

        # Then control for all controlled cells together
        pastedControls <- paste(signifCells2, collapse = ",")
        if (EnrichmentMode == "Linear") {
            if (annotLevel != controlledAnnotLevel) {
                genesCovarData <- utils::read.table(file = genesCovarFile, 
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
            write.table(x = controlledCovarCols2, 
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
                geneCovarFile, 
                controlCovarFile, 
                pastedControlCovars, 
                sumstatsPrefix2)
        }
        magma_run(cmd = magma_cmd, 
                  version = version)
        
        cond_res <- load_magma_results_file(path = sprintf("%s.gsa.out",
                                                           sumstatsPrefix2),
                                            annotLevel =  annotLevel,
                                            ctd = ctd, 
                                            genesOutCOND = NA, 
                                            EnrichmentMode = EnrichmentMode,
                                            ControlForCT = pastedControls)
        allRes <- rbind(allRes, cond_res)

        ## This line makes it so the baseline results 
        ## are appended to the conditonal results
        ctAssocs[[annotLevel]]$results <- rbind(ctAssocs[[annotLevel]]$results,
                                                allRes) 
    }
    ##### Calculate total number of tests performed ####
    totalTests <- 0
    for (annotLevel in seq_len(sum(names(ctAssocs) == ""))) {
        totalTests <- totalTests + dim(ctAssocs[[annotLevel]]$results)[1]
    }
    ctAssocs$total_conditional_tests_performed <- totalTests
    ctAssocs$gwas_sumstats_path <- gwas_sumstats_path
    ctAssocs$analysis_name <- analysis_name
    ctAssocs$upstream_kb <- upstream_kb
    ctAssocs$downstream_kb <- downstream_kb 
    return(ctAssocs)
}
