#' Drop genes within MHC
#'
#' @param geneLocFilePath The gene loc file as downloaded from
#' MAGMA website (e.g. NCBI37.3.gene.loc).
#' @param geneLocFilePathOut The file path to write the gene
#' loc file with MHC removed.
#'
#' @return Null (the output is written to the filepath provided as an argument).
#'
#' @examples
#' gene_loc <- MAGMA.Celltyping:::get_genomeLocFile(build = "GRCH37")
#' MAGMA.Celltyping::drop_genes_within_mhc(geneLocFilePath = gene_loc,
#'                                         geneLocFilePathOut = gene_loc)
#' @export
#' @importFrom utils read.table
#' @importFrom utils write.table
drop_genes_within_mhc <- function(geneLocFilePath,
                                  geneLocFilePathOut) {
    ncbi <- utils::read.table(geneLocFilePath)
    ncbi <- ncbi[!(((ncbi$V3 > 25000000 & ncbi$V3 < 34000000) |
        (ncbi$V4 > 25000000 & ncbi$V4 < 34000000)) &
        ncbi$V2 == 6), ]
    utils::write.table(
        x = ncbi,
        file = geneLocFilePathOut,
        quote = FALSE,
        row.names = FALSE, 
        col.names = FALSE, 
        sep = "\t"
    )
}

drop.genes.within.mhc <- function(...){
    .Deprecated("drop_genes_within_mhc")
    drop_genes_within_mhc(...)
}
