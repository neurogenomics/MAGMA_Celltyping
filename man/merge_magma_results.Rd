% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_magma_results.r
\name{merge_magma_results}
\alias{merge_magma_results}
\title{Merge MAGMA results}
\usage{
merge_magma_results(ctAssoc1, ctAssoc2)
}
\arguments{
\item{ctAssoc1}{Output from either 
\link[MAGMA.Celltyping]{calculate_celltype_associations} or 
\link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.}

\item{ctAssoc2}{Output from either 
\link[MAGMA.Celltyping]{calculate_celltype_associations} or 
\link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.}
}
\value{
List with same format as output from either 
\link[MAGMA.Celltyping]{calculate_celltype_associations} or
\link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
}
\description{
Used for instance when you want to merge results from 
'Linear' and 'Top 10\%' enrichment modes.
}
\examples{
res <- MAGMA.Celltyping::enrichment_results
ctAssoc1 <- res[[1]]$ctAssocsLinear
ctAssoc2 <- res[[1]]$ctAssocsTop
ctAssocs <- merge_magma_results(ctAssoc1=ctAssoc1, ctAssoc2=ctAssoc2)
}
