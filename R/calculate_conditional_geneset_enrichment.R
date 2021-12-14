#' Use MAGMA to test enrichment in a geneset
#'
#' Assumes that you have already run \link[MAGMA.Celltyping]{map_snps_to_genes}.
#'
#' @param controlledAnnotLevel Annotation level of the celltypes
#' being controlled for.
#' @param controlledCTs Array of the celltype to be controlled for,
#' e.g. c("Interneuron type 16","Medium Spiny Neuron).
#' @inheritParams celltype_associations_pipeline
#' @inheritParams calculate_celltype_associations
#' @inheritParams calculate_geneset_enrichment
#'
#' @return File path for the genes.out file.
#' 
#' @examples
#' ### UNDER CONSTRUCTION
#' \dontrun{
#' #### Import data ####
#' ctd <- MAGMA.Celltyping::get_ctd("ctd_allKI")
#' magma_dir <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298")
#' geneset <- MAGMA.Celltyping::rbfox_binding
#' 
#' res <- MAGMA.Celltyping::calculate_conditional_geneset_enrichment(
#'     geneset = geneset,
#'     ctd = ctd,
#'     controlledAnnotLevel = 1,
#'     controlledCTs = "pyramidal SS",
#'     magma_dir = magma_dir,
#'     analysis_name = "Rbfox_16_pyrSS",  
#'     geneset_species = "mouse", 
#'     ctd_species = "mouse")
#' }
#' @export
#' @importFrom data.table data.table
#' @importFrom stats pnorm
#' @importFrom orthogene convert_orthologs
calculate_conditional_geneset_enrichment <- function(geneset,
                                                     ctd,
                                                     ctd_species = "mouse",
                                                     controlledAnnotLevel = 1,
                                                     controlledCTs,
                                                     prepare_ctd = TRUE,
                                                     gwas_sumstats_path = NULL,
                                                     magma_dir = NULL,
                                                     analysis_name = "MainRun",
                                                     upstream_kb = 35,
                                                     downstream_kb = 10, 
                                                     geneset_species = "mouse",
                                                     version = NULL,
                                                     verbose = TRUE
                                                    ) {
    #### Check MAGMA installation ####
    magma_check(version = version, 
                verbose = verbose)
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
        #### Standardise cell-type names ####
        controlledCTs <- EWCE::fix_celltype_names(celltypes = controlledCTs)
    } 
    #### Setup paths ####
    gwas_sumstats_path <- path.expand(gwas_sumstats_path)
    magmaPaths <- get_magma_paths(
        gwas_sumstats_path = gwas_sumstats_path,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )
    #### Convert geneset orthologs ####
    if(geneset_species != "human"){
        gene_map <- orthogene::convert_orthologs(
            gene_df = geneset,
            gene_output = "columns",
            input_species = geneset_species, 
            output_species = "human",
            method = "homologene",
            verbose = verbose)
        geneset <- gene_map$ortholog_gene
    } 
    ##### First, check that the genes are HGNC/MGI IDs ####
    check_entrez_genes(geneset = geneset)
    ### hgnc2entrez_orthogene is a built-in dataset ####
    geneset_entrez <- MAGMA.Celltyping::hgnc2entrez_orthogene[
        MAGMA.Celltyping::hgnc2entrez_orthogene$hgnc_symbol %in% geneset,
    ]$entrez
    #### Check for errors in arguments ####
    check_inputs_to_magma_celltype_analysis(
        ctd = ctd,
        gwas_sumstats_path = gwas_sumstats_path,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )
    #### Check controlled cell type names ####
    check_controlledCTs(ctd = ctd,
                        controlledCTs = controlledCTs,
                        controlledAnnotLevel = controlledAnnotLevel)
    #### Write cell type specificity to disk ####
    ## (so it can be read by MAGMA)
    quantDat2 <- map_specificity_to_entrez(
        ctd = ctd,
        annotLevel = controlledAnnotLevel,
        ctd_species = ctd_species
    )
    geneCovarFile <- tempfile()
    write.table(
        x = quantDat2,
        file = geneCovarFile,
        quote = FALSE,
        row.names = FALSE,
        sep = "\t"
    )
    ctrldCTs <- gsub(" ", ".", paste(controlledCTs, collapse = ","))
    #### Drop any genes from geneset which do not have matching entrez in ctd
    geneset_entrez2 <- geneset_entrez[geneset_entrez %in% quantDat2$entrez]
    ctRows <- paste(c(analysis_name, geneset_entrez2), collapse = " ")
    ##### Write geneset file to disk ####
    ## (so it can be read by MAGMA) 
    geneSetFile <- tempfile()
    write.table(
        x = ctRows,
        file = geneSetFile,
        quote = FALSE,
        row.names = FALSE,
        sep = "\t",
        col.names = FALSE
    )
    #### Run conditional analysis ####
    out_prefix <- paste(c(magmaPaths$filePathPrefix,
                          analysis_name),collapse=".") 
    magma_cmd_cond <- sprintf(
       paste( "magma",
              "--gene-results '%s.genes.raw'",
              "--set-annot '%s'",
              "--gene-covar '%s'",
              "--model direction=positive condition='%s'",
              "--out '%s'"),
        magmaPaths$filePathPrefix,
        geneSetFile,
        geneCovarFile,
        ctrldCTs,
        out_prefix
    )
    magma_run(cmd = magma_cmd_cond, 
              version = version,
              verbose = verbose)
    
    #### Import results #### 
    res <- magma_read_sets_out(out_prefix = out_prefix)
    res_cond <- magma_read_gsa_out(out_prefix = out_prefix, 
                                   analysis_name = analysis_name)
    # Calculate significance of difference
    # between baseline and conditional analyses
    z <- (as.numeric(res$BETA) -
        as.numeric(res_cond$BETA)) /
        sqrt(as.numeric(res$SE)^2 + as.numeric(res_cond$SE)^2)
    p <- 2 * stats::pnorm(-abs(z))

    # Convert to one sided probability
    # (that the conditional analysis is LESS significant than the baseline)
    if (res$BETA > res_cond$BETA) {
        pOneSided <- p / 2
    } else {
        pOneSided <- 1 - p / 2
    }
    #### Gather results into data.table ####
    full_res <- data.table::data.table(
        SET = res$SET,
        NGENES = res$NGENES,
        BASELINE_BETA = res$BETA,
        BASELINE_BETA_STD = res$BETA_STD,
        BASELINE_SE = res$SE,
        BASELINE_P = res$P,
        COND_BETA = res_cond$BETA,
        COND_BETA_STD = res_cond$BETA_STD,
        COND_SE = res_cond$SE,
        COND_P = res_cond$P,
        conditionedCTs = ctrldCTs,
        z = z,
        p_twoSided = p,
        p_oneSided = pOneSided
    )
    return(full_res)
}
