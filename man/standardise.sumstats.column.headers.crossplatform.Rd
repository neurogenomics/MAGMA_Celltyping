% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/standardise.sumstats.column.headers.crossplatform.R
\name{standardise.sumstats.column.headers.crossplatform}
\alias{standardise.sumstats.column.headers.crossplatform}
\title{Standardise the column headers in the Summary Statistics files
 (CROSSPLATFORM)}
\usage{
standardise.sumstats.column.headers.crossplatform(first_line)
}
\arguments{
\item{first_line}{String containing the first line of the sumstats file}
}
\value{
The amended column headers
 (also the column headers will be written directly
  into the summary statistics file)
}
\description{
Use a reference data table of common column header names
 (stored in sumstatsColHeaders.rda) convert them to a standard set,
  i.e. chromosome --> CHR
}
\details{
This function does not check that all the required column headers are present

The amended header is written directly back into the file
}
\examples{
\dontrun{
path <- MAGMA.Celltyping::get_example_gwas()
first_line <- readLines(path)[1]
col_headers <- MAGMA.Celltyping:::standardise.sumstats.column.headers.crossplatform(
     first_line = first_line)
}
}
\keyword{internal}
