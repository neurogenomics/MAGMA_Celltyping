#' Check controlled cell type names
#' 
#' Check the cell types in 'controlledCT' exist
#' at the relevant annotation level.
check_controlledCTs <- function(ctd,
                                controlledCTs,
                                controlledAnnotLevel){
    for (i in seq_len(length(controlledCTs))) {
        annotLvlCTs <- colnames(ctd[[
            as.numeric(controlledAnnotLevel)]]$specificity)
        reqCT <- controlledCTs[[i]]
        if (!reqCT %in% annotLvlCTs) {
            stopper("Cell type specified by controlledAnnotLevel",
                    "cannot be found in ctd level",controlledAnnotLevel,":",
                    reqCT) 
        }
    }
}