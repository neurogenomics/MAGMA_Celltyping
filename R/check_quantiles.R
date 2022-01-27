#' Check quantiles
#' 
#' Check whether every column in every matrix in every level of the 
#' CellTypeDataset (\code{ctd}) has the expected number of quantile bins. 
#' 
#' @keywords internal
check_quantiles <- function(ctd,
                            matrix_name,
                            numberOfBins, 
                            metric = c("n","max")){
    # col_maxes <- apply(ctd_1lvl[[new_matrix_name]], 2, max)\
    #### Check number of unique non-zero quantile values in each col ####
    quantile_counts <- lapply(seq_len(length(ctd)),
                              function(lvl,
                                       .matrix_name=matrix_name,
                                       .metric=metric[1]){
        messager("Checking CTD: level ",lvl,v=TRUE, parallel = TRUE)
        ctd_1lvl <- ctd[[lvl]]
        if(.metric=="n"){
            col_unique <- apply(ctd_1lvl[[.matrix_name]], 2,
                                function(x){length(unique(x[x!=0])) } )
        } else if (.metric=="max") {
            col_unique <- apply(ctd_1lvl[[.matrix_name]], 2, max)
        } 
        bad_cols <- col_unique[col_unique<numberOfBins]
        if(length(bad_cols)>0){
            messager(
                "WARNING: ",length(bad_cols),
                " columns (cell-types) have less than the expected number",
                " of quantile bins ",paste0("(",numberOfBins,").\n"),
                "This may be due to an excessive sparsity or",
                " insufficient variation in your CellTypeDataset.\n",
                # "Problematic columns:\n",
                # paste0("   - ",names(bad_cols)," = ",unname(bad_cols)," bins",
                #        collapse = "\n"),
                v = TRUE,
                parallel = TRUE)
        }
        return(col_unique)
    })
    return(quantile_counts)
}
