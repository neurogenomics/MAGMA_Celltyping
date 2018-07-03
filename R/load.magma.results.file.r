#' Load a MAGMA .gcov.out file
#'
#' Convenience function which just does a little formatting to make it easier to use
#'
#' @param path Path to a .gcov.out file
#' @param annotLevel Which annotation level does this .gcov.out file relate to?
#' @param ctd Cell type data strucutre containing $quantiles
#'
#' @return Contents of the .gcov.out file
#'
#' @examples
#' res = load.magma.results.file(path="Raw/adhd_formtcojo.txt.NEW.10UP.1DOWN.gov.out",annotLevel=1,ctd=ctd)
#'
#' @export
load.magma.results.file <- function(path,annotLevel,ctd){
  library(dplyr)
  res = read.table(path,stringsAsFactors = FALSE)
  colnames(res) = as.character(res[1,])
  res$level=annotLevel
  res=res[-1,]
  
  # Do some error checking
  numCTinCTD = length(colnames(ctd[[annotLevel]]$specificity))
  numCTinRes = dim(res)[1]
  if(numCTinCTD!=numCTinRes){stop(sprintf("%s celltypes in ctd but %s in results file. Did you provide the correct annotLevel?",numCTinCTD,numCTinRes))}
  
  res$COVAR = colnames(ctd[[annotLevel]]$specificity)
  rownames(res) = res$COVAR
  res$BETA = as.numeric(res$BETA)
  res$BETA_STD = as.numeric(res$BETA_STD)
  res$SE = as.numeric(res$SE)
  res$P = as.numeric(res$P)
  res$Method = "MAGMA"
  res$GCOV_FILE = basename(path)
  res$log10p = log(res$P,10)
  
  res = res %>% dplyr::rename(Celltype=COVAR)
  return(res)
}