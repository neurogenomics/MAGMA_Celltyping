#' Get MAGMA OS suffix
#' 
#' Get the corresponding MAGMA executable file suffix for a given OS.
#' 
#' @param os Operating System.
#' 
#' @keywords internal 
magma_os_suffix <- function(os=get_os()) { 
    os_suffix <- switch(EXPR = os,
        Mac = "_mac",
        Windows = "_win",
        Linux = "",
        NULL = ""
    )
    return(os_suffix)
}
