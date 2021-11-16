check_celltype_names <- function(ct_names) {
    bad_names <- ct_names[grepl(" ", ct_names)]
    if (length(bad_names) > 0) {
        messager(
            "Warning:", length(bad_names),
            "cell-type names contained spaces.",
            "Spaces be replaced with '_'."
        )
        ct_names <- gsub(" ", "_", ct_names)
    }
    return(ct_names)
}
