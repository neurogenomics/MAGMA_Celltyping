#' Use MAGMA to test enrichment in a geneset
#'
#' Assumes that you have already run \link[MAGMA.Celltyping]{map_snps_to_genes}.
#'
#' @param geneset Genes which are to be tested (as HGNC / MGI symbols).
#' @param geneset_species Species name relevant to the genes in the geneset.
#' @param controlledAnnotLevel Annotation level of the celltypes
#' being controlled for.
#' @param controlledCTs Array of the celltype to be controlled for,
#' e.g. c("Interneuron type 16","Medium Spiny Neuron).
#' @inheritParams celltype_associations_pipeline
#' @inheritParams calculate_celltype_associations
#'
#' @return File path for the genes.out file.
#' 
#' @examples 
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
#'
#' @export
#' @importFrom data.table data.table
#' @importFrom stats pnorm
calculate_conditional_geneset_enrichment <- function(geneset,
                                                     ctd,
                                                     ctd_species = "mouse",
                                                     prepare_ctd = TRUE,
                                                     controlledAnnotLevel = 1,
                                                     controlledCTs,
                                                     gwas_sumstats_path = NULL,
                                                     magma_dir = NULL,
                                                     analysis_name = "MainRun",
                                                     upstream_kb = 35,
                                                     downstream_kb = 10,
                                                     genome_ref_path = NULL,
                                                     geneset_species = "mouse",
                                                     verbose = TRUE
                                                    ) {
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
    #### Prepare genome_ref ####
    genome_ref_path <- get_genome_ref(
        genome_ref_path = genome_ref_path,
        verbose = verbose
    )
    #### Setup paths ####
    gwas_sumstats_path <- path.expand(gwas_sumstats_path)
    magmaPaths <- get_magma_paths(
        gwas_sumstats_path = gwas_sumstats_path,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )
    
    if(geneset_species != "human"){
        gene_map <- orthogene::convert_orthologs(gene_df = geneset,
                                                 gene_output = "columns",
                                                 input_species = geneset_species, 
                                                 output_species = "human",
                                                 method = "homologene",
                                                 verbose = verbose)
        geneset <- gene_map$ortholog_gene
    } 
    ##### First, check that the genes are HGNC/MGI IDs ####
    n_valid <- sum(
        geneset %in% hgnc2entrez_orthogene$hgnc_symbol)
    if ((n_valid / length(geneset)) < 0.5) {
        stopper(
            "<50% of the geneset are recognised HGNC symbols",
            "with corresponding Entrez IDs.",
            "Check that ctd_species and geneset_species are set correctly."
        )
    }
    ### all_hgnc_wtEntrez is a built-in dataset ####
    geneset_entrez <- hgnc2entrez_orthogene[
        hgnc2entrez_orthogene$hgnc_symbol %in% geneset,
    ]$entrez
    #### Check for errors in arguments ####
    check_inputs_to_magma_celltype_analysis(
        ctd = ctd,
        gwas_sumstats_path = gwas_sumstats_path,
        analysis_name = analysis_name,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb,
        genome_ref_path = genome_ref_path
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
    message_cmd(magma_cmd_cond)
    system(magma_cmd_cond)
    
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
