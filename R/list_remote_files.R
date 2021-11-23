#' List files  
#' 
#' List files on a remote server or website
#'
#' @param URL Web URL.
#' @param pattern Search pattern passed to \link[XML]{xpathSApply}.
#'
#' @source \href{https://stackoverflow.com/questions/15954463/read-list-of-file-names-from-web-into-r}{
#' stackoverflow}
#' 
#' @return List of remote files.
#' @keywords internal
#' @importFrom XML htmlParse xpathSApply free
list_remote_files <- function(URL,
                              pattern = "*.zip") {
    # URL <- "https://ctg.cncr.nl/software/MAGMA/prog/"
    doc <- XML::htmlParse(readLines(URL), asText = TRUE)
    links <- XML::xpathSApply(doc, "//a/@href")
    XML::free(doc)
    wanted <- links[grepl(pattern = pattern, links)]

    GetMe <- file.path(URL, unname(wanted))
    names(GetMe) <- basename(GetMe)
    return(GetMe)
}
