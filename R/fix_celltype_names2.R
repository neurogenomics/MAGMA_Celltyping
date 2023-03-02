#' Fix celltype names
#'
#' Make sure celltypes don't contain characters that could interfere with
#' downstream analyses. For example, the R package
#' \href{https://github.com/neurogenomics/MAGMA_Celltyping}{MAGMA.Celltyping}
#' cannot have spaces in celltype names because spaces are used as a delimiter
#' in later steps.
#'
#' @param celltypes Character vector of celltype names.
#' @param replace_chars Regex string of characters to replace
#'  with "_" when renaming columns.
#' @param make_unique Make all entries unique.
#' @returns Fixed celltype names.
#'
#' @keywords internal 
fix_celltype_names2 <- function(celltypes,
                               replace_chars = "[-]|[.]|[ ]|[//]|[\\/]",
                               make_unique = TRUE) {
    if (is.null(celltypes)) {
        return(NULL)
    }
    celltypes <- gsub(replace_chars, "_", celltypes)
    ### Remove repeating "_" ####
    celltypes <- gsub("[_]+", "_", celltypes)
    #### Make sure all are unique ####
    if(isTRUE(make_unique)){
        celltypes <- make.unique(celltypes)   
    }
    return(celltypes)
}
