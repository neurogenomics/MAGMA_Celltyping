standardise_ctd <- function(ctd, lvl) {
    #### Need to make sure colnames still match after theyre edited by prepare_quantile_groups step
    message("+ Standardising CTD.")
    for (nm in names(ctd[[lvl]])) {
        if ((!nm %in% c("annot", "plotting")) & (!is.null(dim(ctd[[lvl]][[nm]])))) {
            ctd[[lvl]][[nm]] <- as.matrix(data.frame(as.matrix(ctd[[lvl]][[nm]])))
            colnames(ctd[[lvl]][[nm]]) <- make.unique(colnames(ctd[[lvl]][[nm]]))
        }
    }
    return(ctd)
}
