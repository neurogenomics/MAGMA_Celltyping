.onAttach <- function(libname, pkgname) {
    packageStartupMessage(paste0(
        "===== Welcome to MAGMA.Celltyping =====\n",
        "This package depends on the MAGMA command line tool:\n",
        "https://ctg.cncr.nl/software/magma\n",
        "===== =========================== =====\n"
    ))
}

# .onLoad <- function(libname, pkgname) {
#     if (is.null(magma_installed_version())) {
#         dest_magma <- magma_install()
#     }
# }