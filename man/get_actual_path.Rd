% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_actual_path.R
\name{get_actual_path}
\alias{get_actual_path}
\title{Get actual path}
\usage{
get_actual_path(path)
}
\arguments{
\item{path}{File path.}
}
\description{
When given a file path, check that it exists and if not, search for
 similarly named files and return those instead. 
This is important because sometimes MAGMA adds a .txt. at the end of 
output files (especially on Windows).
}
\keyword{internal}
