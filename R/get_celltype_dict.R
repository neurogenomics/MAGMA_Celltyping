get_celltype_dict <- function(all_df) {
    ##### Celltype names get changed during MAGMA_Celltyping. Translate here
    tmp <- all_df
    tmp$dummy <- 1
    tmp <- Matrix::t(unique(tmp[, c("Celltype", "dummy")]))
    colnames(tmp) <- tmp[1, ]
    tmp <- data.frame(tmp)
    celltype_dict <- stats::setNames(colnames(tmp), tmp[1, ])
    return(celltype_dict)
}
