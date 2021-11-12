#' Drop genes within MHC
#'
#' @param geneLocFilePath The gene loc file as downloaded from
#' MAGMA website (e.g. NCBI37.3.gene.loc).
#' @param geneLocFilePathOut The file path to write the gene
#' loc file with MHC removed.
#'
#' @return NULL (the output is written to the filepath provided as an argument)
#'
#' @examples
#' drop.genes.within.mhc("data/NCBI37.3.gene.loc", "data/NCBI37.3.gene.loc")
#' drop.genes.within.mhc("data/NCBI38.gene.loc", "data/NCBI38.gene.loc")
#' drop.genes.within.mhc("data/NCBI36.3.gene.loc", "data/NCBI36.3.gene.loc")
#' @export
#' @importFrom utils read.table
#' @importFrom utils write.table
drop.genes.within.mhc <- function(geneLocFilePath, geneLocFilePathOut) {
    # chr6 25-34 mb
    # ncbi = read.table("data/NCBI37.3.gene.loc")
    # ncbi_wo = read.table(
    #     "/Users/natske/Downloads/NCBI37.3.gene.loc (1).extendedMHCexcluded")
    # ncbi6 = ncbi[ncbi$V2==6,]
    # ncbi_wo6 = ncbi_wo[ncbi_wo$V2==6,]
    ncbi <- utils::read.table(geneLocFilePath)
    ncbi <- ncbi[!(((ncbi$V3 > 25000000 & ncbi$V3 < 34000000) |
        (ncbi$V4 > 25000000 & ncbi$V4 < 34000000)) &
        ncbi$V2 == 6), ]
    utils::write.table(ncbi,
        file = geneLocFilePathOut,
        quote = FALSE,
        row.names = FALSE, col.names = FALSE, sep = "\t"
    )
}
