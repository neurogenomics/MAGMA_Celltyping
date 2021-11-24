#' Checks whether OS is a 32-bit Windows
#'
#' Helper function to avoid duplicate test runs on Windows OS.
#'
#' @return Null
#'
#' @keywords internal
is_32bit <- function() {
    is_32bit_windows <- .Platform$OS.type == "windows" &&
        .Platform$r_arch == "i386"
    return(is_32bit_windows)
}
