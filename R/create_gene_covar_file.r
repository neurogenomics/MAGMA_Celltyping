#' Create gene covariance file
#'
#' The gene covariannce (covar) file is the input to MAGMA for the
#' celltype association analysis.
#' This code was functonalised because it is called by both
#' baseline and conditional analysis.
#'
#' @param genesOutFile The output of the second call to MAGMA
#' (performed in the \link[MAGMA.Celltyping]{map_snps_to_genes} function).
#' @param ctd Cell type data structure. Must contain quantiles.
#' @param annotLevel Annot level for which the gene covar file
#' should be constructed
#' @param ctd_species Species name relevant to the cell type data,
#'  i.e. "mouse" or "human"
#' @param genesOutCOND [Optional] Path to a \emph{genes.out}
#'  file to condition on. Used if you want to condition on a different GWAS.
#' @param verbose Print messages.
#' @source
#' \code{
#' #### Example usage ####
#' ctd <- ewceData::ctd()
#' genesOutFile <- MAGMA.Celltyping::import_magma_files(
#'     ids = "ieu-a-298",
#'     file_types = ".genes.out$",
#'     return_dir = FALSE)
#'
#' genesCovarFilePath <- MAGMA.Celltyping:::create_gene_covar_file(
#'     genesOutFile = genesOutFile,
#'     ctd = ctd,
#'     annotLevel = 1,
#'     ctd_species = "mouse")
#' }
#'
#' @return File path for the gene covar file.
#'
#' @keywords internal
#' @importFrom utils read.table write.table
create_gene_covar_file <- function(genesOutFile,
                                   ctd,
                                   annotLevel,
                                   ctd_species,
                                   genesOutCOND = NA,
                                   verbose = TRUE) { 
    quantDat2 <- map_specificity_to_entrez(
        ctd = ctd,
        annotLevel = annotLevel,
        ctd_species = ctd_species, 
        use_matrix = "specificity_quantiles",
        verbose = verbose
    ) 
    if (dim(quantDat2)[1] < 100) {
        stop_msg <- paste(
            "Less than one hundred genes detected after",
            "mapping genes between species.",
            "Was ctd_species defined correctly?"
        )
        stop(stop_msg)
    }

    # Read in the genes.out file (which has a p-value for each entrez gene)
    # genesOut = read.table(genesOutFile,stringsAsFactors = FALSE)

    # If the analysis is being run conditionally on another GWAS
    if (!is.na(genesOutCOND[1])) {
        for (i in seq_len(length(genesOutCOND))) {
            genesOutCOND_data <- read.table(
                file = genesOutCOND[i],
                stringsAsFactors = FALSE, 
                header = TRUE,
            )[, c("GENE", "ZSTAT")]
            colnames(genesOutCOND_data)[1] <- "entrez" 
            ## Expand the entrez definitions to include other entrez symbols
            ## matching the relevant gene symbols.  
            genesOutCOND_data2 <- merge(
                x = MAGMA.Celltyping::hgnc2entrez_orthogene,
                y = genesOutCOND_data,
                by = "entrez"
            )[, c(1, 3)] 
            colnames(genesOutCOND_data2)[2] <- sprintf("ZSTAT%s", i)  
            quantDat2 <- merge(quantDat2, 
                               genesOutCOND_data2, 
                               by = "entrez")
        }
    }
    #### Ensure there are no duplicate genes ####
    dup_genes <- duplicated(quantDat2$entrez)
    if(sum(dup_genes, na.rm = TRUE)>0){
        messager("Removing", sum(dup_genes, na.rm = TRUE),
                 "gene(s) with duplicate entrez IDs.",v=verbose)
        quantDat2 <- quantDat2[!dup_genes,]
    }
    #### Ensure there are no duplicate cell types when ignoring case (which MAGMA does) ####
    dup_ct <- duplicated(toupper(colnames(quantDat2)))
    if(sum(dup_ct)>0){
      messager("Removing", sum(dup_ct, na.rm = TRUE),
               "cell type(s) with duplicate cell types (when ignoring upper/lowercase):",
               paste(shQuote(colnames(quantDat2)[dup_ct]), collapse = ","),
               "Please consider regenerating your CTD object with",
               "unique cell type names (when ignoring upper/lowercase)",
               "for more accurate results.",
               v=verbose)
      quantDat2 <- quantDat2[,!dup_ct]
    }
    #### Write genes covar file to disk ####
    geneCovarFile <- tempfile()
    utils::write.table(
        x = quantDat2,
        file = geneCovarFile,
        quote = FALSE,
        row.names = FALSE,
        sep = "\t"
    )
    return(geneCovarFile)
}
