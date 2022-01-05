#' Create a fake GWAS path when all you have are the MAGMA files
#' 
#' Necessary to trick 
#' \link[MAGMA.Celltyping]{calculate_celltype_associations} and 
#' \link[MAGMA.Celltyping]{calculate_conditional_celltype_associations} to 
#' take MAGMA files (".genes.raw" and ".genes.out") alone.
#' 
#' @inheritParams celltype_associations_pipeline
#' @keywords internal
create_fake_gwas_path <- function(magma_dir,
                                  upstream_kb = 35,
                                  downstream_kb = 10,
                                  remove_pattern="MAGMA_Files"){
    fake_gwas_ss <- file.path(
        gsub(remove_pattern, "", dirname(magma_dir)),
        gsub(
            paste0(".", upstream_kb, "UP.", downstream_kb, "DOWN"), "",
            basename(magma_dir)
        )
    )
    fake_gwas_ss <- fix_path(fake_gwas_ss)
    return(fake_gwas_ss)
}