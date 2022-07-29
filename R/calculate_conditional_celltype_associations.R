#' Calculate conditional celltype associations using MAGMA
#'
#' Run cell-type enrichment analysis on a GWAS previously mapped to genes 
#' (using \link[MAGMA.Celltyping]{map_snps_to_genes}) while controlling for 
#' certain cell-types. This allows one to conduct cell-type enrichment
#'  analyses while controlling for the strongest cell-type-specific signatures.   
#' Which cell-types are controlled for can be specified by
#'  either of the following arguments: 
#' \itemize{
#'  \item{\code{controlTopNcells}}{Automatically 
#' selects the top N mostly significantly enriched cell-types}.  
#'  \item{\code{controlledCTs}}{A user-provided list of cell-types 
#'  present in the \code{ctd}}. 
#' } 
#' Three sets of analyses are run:  
#' \itemize{
#'  \item{Baseline enrichment results}{No conditioning.}
#'  \item{Conditional results: separate}{ 
#'  conditioning on each specified cell-type separately}
#'  \item{Conditional results: grouped}{
#'  conditioning all specified cell-types at once.}
#' }
#'
#' @param controlledAnnotLevel Which annotation level should be controlled for.
#' @param controlTopNcells How many of the most significant cell types at
#' that annotation level should be controlled for?
#' @param controlledCTs Array of the celltype to be controlled for,
#' e.g. \code{c('Interneuron type 16','Medium Spiny Neuron')}. 
#' @inheritParams celltype_associations_pipeline
#' @inheritParams calculate_celltype_associations
#'
#' @returns A concatenated results table containing:
#' \itemize{
#'  \item{Baseline enrichment results}. 
#'  \item{Conditional results: conditioning on each 
#'  specified cell-type individually}.
#'  \item{Conditional results: conditioning all specified cell-types at once}.
#' }
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
    ctd_species = infer_ctd_species(ctd),
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
        controlledCTs <- EWCE::fix_celltype_names(controlledCTs)
    } 
    #### Setup paths ####
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
    if (!is.na(controlTopNcells) && !all(is.na(controlledCTs))) {
        stopper(
            "Either controlTopNcells or controlledCTs",
            "should be passed with arguments, not both."
        )
    }
    #### If both are NA then also reject that ####
    if (is.na(controlTopNcells) && all(is.na(controlledCTs))) {
        stopper(
            "Either controlTopNcells or controlledCTs",
            "should be passed with arguments."
        )
    }
    ##### Calculate the baseline associations ####
    ctAssocs <- calculate_celltype_associations(
        ctd = ctd,
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

    if (!any(is.na(controlledCTs))) {
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

    #### Create gene covar file for the controlled for annotation level ####
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
    
    #### Iterate over CTD levels ####
    for (annotLevel in seq_len(length(ctd))) {
        #### Create gene-covariacne file ####
        # First match quantiles to the genes in the genes.out file...
        # then write as the genesCovar file (the input to MAGMA)
        genesCovarFile <- create_gene_covar_file_mode(
            EnrichmentMode = EnrichmentMode,
            magmaPaths = magmaPaths,
            ctd = ctd, 
            annotLevel = annotLevel, 
            ctd_species = ctd_species)
        #### Iterate over celltypes ####
        ##### First control for each individually #####  
        allRes <- iterate_conditional_celltypes(
            ctd=ctd,
            EnrichmentMode=EnrichmentMode,
            signifCells2=signifCells2,
            annotLevel=annotLevel,
            controlledAnnotLevel=controlledAnnotLevel,
            genesCovarFile=genesCovarFile,
            controlCovarFile=controlCovarFile,
            controlledCovarCols=controlledCovarCols, 
            magmaPaths=magmaPaths, 
            upstream_kb=upstream_kb, 
            downstream_kb=downstream_kb,
            version=version)
        ##### Then control for all controlled cells together ##### 
        allRes <- iterate_conditional_celltypes_grouped(
            allRes=allRes,
            ctd=ctd,
            signifCells2=signifCells2,
            EnrichmentMode=EnrichmentMode, 
            annotLevel=annotLevel,
            controlledAnnotLevel=controlledAnnotLevel,
            genesCovarFile=genesCovarFile,
            controlCovarFile=controlCovarFile,
            controlledCovarCols=controlledCovarCols,
            magmaPaths=magmaPaths,
            upstream_kb=upstream_kb, 
            downstream_kb=downstream_kb,
            version=version)
        ## This line makes it so the baseline results 
        ## are appended to the conditional results
        ctAssocs[[annotLevel]]$results <- rbind(ctAssocs[[annotLevel]]$results,
                                                allRes) 
    } #### End for loop over CTD levels
    
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
