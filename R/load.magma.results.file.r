#' Load a MAGMA .gcov.out file
#'
#' Convenience function which just does a little formatting to make it easier to use
#'
#' @param path Path to a .gcov.out file (if EnrichmentMode=='Linear') or .sets.out (if EnrichmentMode=='Top 10%')
#' @param annotLevel Which annotation level does this .gcov.out file relate to?
#' @param ctd Cell type data strucutre containing $quantiles
#' @param genesOutCOND [Optional] If the analysis controlled for another GWAS, then this is included as a column, otherwise the column is NA
#' @param EnrichmentMode [Optional] Either 'Linear' or 'Top 10%'. Default assumes Linear.
#'
#' @return Contents of the .gcov.out file
#'
#' @examples
#' res = load.magma.results.file(path="Raw/adhd_formtcojo.txt.NEW.10UP.1DOWN.gov.out",annotLevel=1,ctd=ctd)
#'
#' @export
load.magma.results.file <- function(path,annotLevel,ctd,genesOutCOND=NA,EnrichmentMode="Linear",ControlForCT="BASELINE"){
  # Check EnrichmentMode has correct values
  if(!EnrichmentMode %in% c("Linear","Top 10%")){stop("EnrichmentMode argument must be set to either 'Linear' or 'Top 10%")}
    
  # Check the file has appropriate ending given EnrichmentMode    
  if(EnrichmentMode=="Linear" & length(grep(".gcov.out$",path))==0){stop("If EnrichmentMode=='Linear' then path must end in .gcov.out")}    
  if(EnrichmentMode=="Top 10%" & length(grep(".sets.out$",path))==0){stop("If EnrichmentMode=='Top 10%' then path must end in .sets.out")}    
    
  library(dplyr)
  res = read.table(path,stringsAsFactors = FALSE)
  colnames(res) = as.character(res[1,])
  res$level=annotLevel
  res=res[-1,]
  
  # Do some error checking
  numCTinCTD = length(colnames(ctd[[annotLevel]]$specificity))
  numCTinRes = dim(res)[1]
  if(ControlForCT=="BASELINE"){
    if(numCTinCTD!=numCTinRes){stop(sprintf("%s celltypes in ctd but %s in results file. Did you provide the correct annotLevel?",numCTinCTD,numCTinRes))}
      res$COVAR = colnames(ctd[[annotLevel]]$specificity)
  }else{
    #if(numCTinCTD!=(numCTinRes+1)){stop(sprintf("%s celltypes in ctd but %s in results file. Expected %s-1 in results file. Did you provide the correct annotLevel?",numCTinCTD,numCTinRes,numCTinRes))}  
      tmpF = tempfile()
      tmpDat = t(data.frame(original=colnames(ctd[[annotLevel]]$specificity))) ;  colnames(tmpDat) = colnames(ctd[[annotLevel]]$specificity)
      write.csv(tmpDat,file=tmpF)
      editedNames = colnames(read.csv(tmpF,stringsAsFactors = FALSE))[-1]
      transliterateMap = data.frame(original=colnames(ctd[[annotLevel]]$specificity),edited=editedNames,stringsAsFactors = FALSE)
      rownames(transliterateMap) = transliterateMap$edited
      res = res[res$COVAR %in% rownames(transliterateMap),]
      res$COVAR = transliterateMap[res$COVAR,]$original
  }
  
  rownames(res) = res$COVAR
  res$BETA = as.numeric(res$BETA)
  res$BETA_STD = as.numeric(res$BETA_STD)
  res$SE = as.numeric(res$SE)
  res$P = as.numeric(res$P)
  res$Method = "MAGMA"
  res$GCOV_FILE = basename(path)
  res$CONTROL = ControlForCT
  res$CONTROL_label = ControlForCT
  res$log10p = log(res$P,10)
  res$genesOutCOND = genesOutCOND
  res$EnrichmentMode = EnrichmentMode
  res = res %>% dplyr::rename(Celltype=COVAR)
  
  if(EnrichmentMode=="Top 10%"){
    res = res %>%  dplyr::rename(OBS_GENES=NGENES) %>%  purrr::modify_at(c("SET"),~NULL)
    res = res[,c("Celltype","OBS_GENES","BETA","BETA_STD","SE","P","level","Method","GCOV_FILE","CONTROL","CONTROL_label","log10p","genesOutCOND","EnrichmentMode")]
  }
  
  return(res)
}