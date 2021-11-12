#' Get the available versions of MAGMA
#'
#' @source \href{https://ctg.cncr.nl/software/magma}{MAGMA website}
#' @keywords internal
#' @importFrom stringr str_split
magma_versions <- function(links) {
    #### Method 1 ####
    # # The documentation isn't always up to date,
    # # so you also need to check whether there is a more recent version here.
    # doc_url=file.path("https://ctg.cncr.nl/software",
    #                   "MAGMA/doc/changelog.txt")
    # doc <- readLines(doc_url)
    # versions <- grep(paste(paste0("^v",seq(1,10)), collapse = "|"),
    #                  doc,
    #                  value = TRUE)
    # if(latest) return(versions[1]) else return(versions)

    #### Method 2 ####
    #### Find the right MAGMA version for the machine type ####
    versions <- stringr::str_split(names(links), "_|[.]zip",
        simplify = TRUE
    )[, 2]
    versions <- unique(versions)
    return(versions)
}
