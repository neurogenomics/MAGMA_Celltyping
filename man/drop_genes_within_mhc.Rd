% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drop_genes_within_mhc.r
\name{drop_genes_within_mhc}
\alias{drop_genes_within_mhc}
\title{Drop genes within MHC}
\usage{
drop_genes_within_mhc(geneLocFilePath, geneLocFilePathOut)
}
\arguments{
\item{geneLocFilePath}{The gene loc file as downloaded from
MAGMA website (e.g. NCBI37.3.gene.loc).}

\item{geneLocFilePathOut}{The file path to write the gene
loc file with MHC removed.}
}
\value{
Null (the output is written to the filepath provided as an argument).
}
\description{
Drop genes within MHC
}
\examples{
gene_loc <- MAGMA.Celltyping:::get_genomeLocFile(build = "GRCH37")
MAGMA.Celltyping::drop_genes_within_mhc(geneLocFilePath = gene_loc,
                                        geneLocFilePathOut = gene_loc)
}
