check_controlled_celltypes <- function(ctAssocs,
                                   controlledCTs,
                                   controlledAnnotLevel,
                                   controlTopNcells,
                                   qvalue_thresh,
                                   ctd){
    
    if (!any(is.na(controlledCTs))) {
        # Check if controlledCTs are all in the CTD
        # at the expected annotation level
        if (mean(controlledCTs %in% colnames(
            ctd[[controlledAnnotLevel]]$specificity
        )) < 1) {
            missingCTs <- controlledCTs[
                !controlledCTs %in% colnames(
                    ctd[[controlledAnnotLevel]]$specificity
                )
            ]
            stopper(
                "The following celltypes are not found at",
                "the specified annotation level:",
                paste(missingCTs, sep = " ")
            )
        } else {
            signifCells <- controlledCTs
        } 
    # Find the cells which are most significant at baseline
    # at controlled annotation level
    } else {
        res <- ctAssocs[[controlledAnnotLevel]]$results
        res <- res[
            order(res$P,-abs(res$BETA)),
        ] 
        res$Q <- stats::p.adjust(res$P,method = "bonf")
        signifCells <- as.character(
            res[res$Q <= qvalue_thresh,]$Celltype
        ) 
        # If there are no significant cells... then stop
        if (length(signifCells) == 0) {
            messager("Warning: No annotLevel",controlledAnnotLevel,
                     "celltypes reach significance @",
                     paste0("q-value<=",qvalue_thresh),
                     "returning NULL.")
            return(NULL)
        }
        if (length(signifCells) > controlTopNcells) {
            signifCells <- signifCells[seq_len(controlTopNcells)]
        }
    }
   return(signifCells)
}