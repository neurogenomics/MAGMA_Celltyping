#' Use MAGMA to test enrichment in a geneset
#'
#' Assumes that you have already run
#' \link[MAGMA.Celltyping]{map_snps_to_genes}.
#'
#' @param geneset Genes which are to be tested. 
#' Can be gene symbols (or other gene IDs) from any species listed in 
#' \link[EWCE]{list_species}.
#' @param geneset_species Species that the \code{geneset} came from.
#' Can be any species listed in \link[EWCE]{list_species}.
#' If \code{geneset_species!="human"}, the \code{geneset} will be converted
#'  to 1:1 human orthologs using \link[orthogene]{convert_orthologs}.
#' 
#' @inheritParams calculate_celltype_associations
#' @inheritParams celltype_associations_pipeline
#'
#' @returns Filepath for the genes.out file.
#'
#' @export
#' @importFrom utils read.table write.table 
#' @importFrom orthogene convert_orthologs
calculate_geneset_enrichment <- function(geneset,
                                         gwas_sumstats_path = NULL,
                                         magma_dir = NULL,
                                         analysis_name,
                                         upstream_kb = 35,
                                         downstream_kb = 10,
                                         geneset_species = "mouse",
                                         version = NULL,
                                         verbose = TRUE) {
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
    gwas_sumstats_path <- path.expand(gwas_sumstats_path)
    magmaPaths <- get_magma_paths(gwas_sumstats_path = gwas_sumstats_path, 
                                  upstream_kb = upstream_kb, 
                                  downstream_kb = downstream_kb)
    #### Convert geneset orthologs ####
    if(geneset_species != "human"){
        gene_map <- orthogene::convert_orthologs(gene_df = geneset,
                                                 gene_output = "columns",
                                                 input_species = geneset_species, 
                                                 output_species = "human",
                                                 method = "homologene",
                                                 verbose = verbose)
        geneset <- gene_map$ortholog_gene
        geneset_species <- "human"
    } 
    ##### First, check that the genes are HGNC/MGI IDs ####
    check_entrez_genes(geneset = geneset)
    ### hgnc2entrez_orthogene is a built-in dataset ####
    geneset_entrez <- MAGMA.Celltyping::hgnc2entrez_orthogene[
        MAGMA.Celltyping::hgnc2entrez_orthogene$hgnc_symbol %in% geneset,
    ]$entrez

    ctRows <- paste(c(analysis_name, geneset_entrez), collapse = " ")
    #### Write genes covar file to disk ####
    geneCovarFile <- tempfile() 
    utils::write.table(x = ctRows, 
                       file = geneCovarFile, 
                       quote = FALSE, 
                       row.names = FALSE, 
                       sep = "\t",
                       col.names = FALSE)
    #### Run MAGMA ###
    magma_cmd <- sprintf(
        paste("magma",
              "--gene-results '%s.genes.raw'",
              "--set-annot '%s'",
              "--out '%s.%s'" 
        ), magmaPaths$filePathPrefix, 
        geneCovarFile, 
        magmaPaths$filePathPrefix, 
        analysis_name
    )
    magma_run(cmd = magma_cmd, 
              version = version)

    path <- sprintf("%s.%s.gsa.out", magmaPaths$filePathPrefix, analysis_name)
    res <- utils::read.table(file = path, 
                             header = TRUE,
                             stringsAsFactors = FALSE, 
                             check.names = FALSE)  
    return(res)
}
