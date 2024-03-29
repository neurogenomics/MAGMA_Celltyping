% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterate_conditional_celltypes_grouped.R
\name{iterate_conditional_celltypes_grouped}
\alias{iterate_conditional_celltypes_grouped}
\title{iterate_conditional_celltypes: grouped}
\usage{
iterate_conditional_celltypes_grouped(
  allRes,
  ctd,
  signifCells2,
  EnrichmentMode,
  annotLevel,
  controlledAnnotLevel,
  genesCovarFile,
  controlCovarFile,
  controlledCovarCols,
  magmaPaths,
  upstream_kb,
  downstream_kb,
  version,
  verbose
)
}
\arguments{
\item{allRes}{Results from
\code{MAGMA.Celltyping::iterate_conditional_celltypes}.}

\item{ctd}{Cell type data structure containing
\code{specificity_quantiles}.}

\item{EnrichmentMode}{[Optional] Should either 'Linear' or 'Top 10\%' mode
be used for testing enrichment?}

\item{controlledAnnotLevel}{Which annotation level should be controlled for.}

\item{upstream_kb}{How many kb upstream of the gene
should SNPs be included?}

\item{downstream_kb}{How many kb downstream of the gene
should SNPs be included?}

\item{version}{MAGMA version to use.}

\item{verbose}{Print messages.}
}
\description{
Support function for 
\link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
}
\keyword{internal}
