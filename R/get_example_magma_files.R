get_example_magma_files <- function(file_types,
                                    verbose = TRUE){
    
    messager("Using built-in example files: ieu-a-298.tsv.gz.35UP.10DOWN",
             v=verbose)
    magma_dir <-  system.file("extdata/MAGMA_Files/ieu-a-298", 
                              package = "MAGMA.Celltyping")
    local_files <- list.files(magma_dir, full.names = TRUE)
    local_files <- grep(paste(file_types,collapse = "|"),
                        local_files, value = TRUE)
    ## Copy to tempdir and rename to include up/down info 
    ## which MAGMA.Celltyping requires 
    tmpdir <- file.path(tempdir(),"MAGMA_Files",
                        "ieu-a-298.tsv.gz.35UP.10DOWN")
    dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
    temp_paths <-  file.path(tmpdir, gsub("ieu-a-298",
                                          "ieu-a-298.tsv.gz.35UP.10DOWN",
                                          basename(local_files)))
    out <- file.copy(local_files, temp_paths) 
    return(temp_paths)
}
