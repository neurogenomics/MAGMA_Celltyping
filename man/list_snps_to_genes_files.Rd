% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_snps2genes_files.R
\name{list_snps_to_genes_files}
\alias{list_snps_to_genes_files}
\title{List SNPs-to-genes mapping files}
\usage{
list_snps_to_genes_files(save_dir, pattern = "*.genes.out$", verbose = TRUE)
}
\arguments{
\item{save_dir}{Directory to recursively search for matching files in.}

\item{pattern}{an optional \link[base]{regular expression}.  Only file names
    which match the regular expression will be returned.}

\item{verbose}{Print messages.}
}
\value{
Named list of paths.
}
\description{
List paths to all SNPs-to-genes mapping files generated by
\link[MAGMA.Celltyping]{map_snps_to_genes}.
}
