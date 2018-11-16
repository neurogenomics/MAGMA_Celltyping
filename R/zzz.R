.onLoad <- function(libname, pkgname){
    packageStartupMessage("Welcome to MAGMA.Celltyping.\nThis package depends on the use of the magma command line tool which is available from https://ctg.cncr.nl/software/magma.\nTesting if you have it installed and available from the command line")
    #if(!system(sprintf("%smagma",magma_path))==1){stop("ERROR: magma command not found at magma_path")}
    if(!system(sprintf("magma"))==1){
        stop("magma does not appear to be located on your path\nPlease download it from https://ctg.cncr.nl/software/magma\nThe executable should then be copied to /usr/local/bin")
    }else{
        packageStartupMessage("MAGMA succesfully loaded")
    }
}
