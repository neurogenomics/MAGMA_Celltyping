% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.r
\docType{data}
\name{rbfox_binding}
\alias{rbfox_binding}
\title{Rbfox binding genes in MGI format}
\format{
An object of class \code{character} of length 4143.
}
\source{
\code{
    rbfox_binding = read.csv(
        "/Users/natske/Google Drive/DiseaseLists/Rbfox_binding.txt",
        stringsAsFactors = FALSE)[-1,1]
    usethis::use_data(rbfox_binding,overwrite = TRUE)
}
}
\usage{
data("rbfox_binding")
}
\description{
List of significant hits for Rbfox 1 2 or 3 binding from supplementary
table 1 of HITS-CLIP and Integrative Modeling Define the Rbfox
Splicing-Regulatory Network Linked to Brain Development and Autism.
All with rbfox2 count greater than 4 or summed rbfox 1 and 3 greater than 12.
}
\keyword{datasets}
