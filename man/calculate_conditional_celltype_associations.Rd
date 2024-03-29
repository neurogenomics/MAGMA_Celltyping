% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_conditional_celltype_associations.R
\name{calculate_conditional_celltype_associations}
\alias{calculate_conditional_celltype_associations}
\title{Calculate conditional celltype associations using MAGMA}
\usage{
calculate_conditional_celltype_associations(
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
  qvalue_thresh = 0.05,
  force_new = FALSE,
  version = NULL,
  verbose = TRUE
)
}
\arguments{
\item{ctd}{Cell type data structure containing
\code{specificity_quantiles}.}

\item{ctd_species}{Species name relevant to the CellTypeDataset (\code{ctd}).
See \link[EWCE]{list_species} for all available species. 
If \code{ctd_species=NULL} (default),
 the \code{ctd} species will automatically 
 be inferred using \link[orthogene]{infer_species}.}

\item{gwas_sumstats_path}{File path of the summary statistics file.}

\item{magma_dir}{Path to folder containing the
pre-computed MAGMA GWAS files (\emph{.gsa.raw}and \emph{.gsa.out}).}

\item{analysis_name}{Used in file names which area created.}

\item{prepare_ctd}{Whether to run
\link[MAGMA.Celltyping]{prepare_quantile_groups} on the \code{ctd} first.}

\item{upstream_kb}{How many kb upstream of the gene
should SNPs be included?}

\item{downstream_kb}{How many kb downstream of the gene
should SNPs be included?}

\item{controlledAnnotLevel}{Which annotation level should be controlled for.}

\item{controlTopNcells}{How many of the most significant cell types at
that annotation level should be controlled for?}

\item{controlledCTs}{Array of the celltype to be controlled for,
e.g. \code{c('Interneuron type 16','Medium Spiny Neuron')}.}

\item{EnrichmentMode}{[Optional] Should either 'Linear' or 'Top 10\%' mode
be used for testing enrichment?}

\item{qvalue_thresh}{Multiple-testing corrected p-value threshold to filter
by when determining which celltypes to condition with.}

\item{force_new}{[Optional] Force new MAGMA analyses even if the
pre-existing results files are detected.}

\item{version}{MAGMA version to use.}

\item{verbose}{Print messages.}
}
\value{
A concatenated results table containing:
\itemize{
 \item{Baseline enrichment results}. 
 \item{Conditional results: conditioning on each 
 specified cell-type individually}.
 \item{Conditional results: conditioning all specified cell-types at once}.
}
}
\description{
Run cell-type enrichment analysis on a GWAS previously mapped to genes 
(using \link[MAGMA.Celltyping]{map_snps_to_genes}) while controlling for 
certain cell-types. This allows one to conduct cell-type enrichment
 analyses while controlling for the strongest cell-type-specific signatures.   
Which cell-types are controlled for can be specified by
 either of the following arguments: 
\itemize{
 \item{\code{controlTopNcells}}{Automatically 
selects the top N mostly significantly enriched cell-types}.  
 \item{\code{controlledCTs}}{A user-provided list of cell-types 
 present in the \code{ctd}}. 
} 
Three sets of analyses are run:  
\itemize{
 \item{Baseline enrichment results}{No conditioning.}
 \item{Conditional results: separate}{ 
 conditioning on each specified cell-type separately}
 \item{Conditional results: grouped}{
 conditioning all specified cell-types at once.}
}
}
\examples{
#### Prepare cell-type data ####
ctd <- ewceData::ctd()
#### Prepare GWAS MAGMA data ####
magma_dir <- MAGMA.Celltyping::import_magma_files(ids = "ieu-a-298")
#### Run pipeline ####
ctAssocs <- calculate_conditional_celltype_associations(
    ctd = ctd,
    controlledAnnotLevel = 1,
    controlTopNcells = 1,
    qvalue_thresh = 1,
    magma_dir = magma_dir,
    ctd_species = "mouse", 
    force = TRUE) 
}
