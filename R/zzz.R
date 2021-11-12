.onAttach <- function(libname, pkgname) {
    packageStartupMessage(paste0(
        "===== Welcome to MAGMA.Celltyping =====\n",
        "This package depends on the MAGMA command line tool\n",
        "https://ctg.cncr.nl/software/magma\n",
        "===== =========================== =====\n"
    ))
}

.onLoad <- function(libname, pkgname) {
    if (is.null(magma_installed_version())) install_magma() 
    
    # HAD TO COMMENT OUT BELOW CODE EVEN THOUGH IT IS USEFUL
    # BECAUSE IT CAUSES MAGMA TO THROW AN ERROR...
    # WHICH MAKES devtools::check() FAIL
    # else{
    #     ret <- system2("magma","--v", stdout=TRUE, stderr=TRUE)
    #     if(length(grep("v1.07b",ret[1]))==1){
    #         packageStartupMessage("MAGMA succesfully loaded")
    #     }else{
    #         stop("MAGMA_celltyping assumes you have magma v1.07b",
    #              "installed on the path")
    #     }
    # }
}
