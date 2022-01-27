#' Get OS
#' 
#' Infer the Operating System (OS) of the current machine being used. 
#' 
#' @keywords internal
get_os <- function() {
    OS <- ""
    switch(Sys.info()[["sysname"]],
        Windows = {
            OS <- "Windows"
        },
        Linux = {
            OS <- "Linux"
        },
        Darwin = {
            OS <- "Mac"
        }
    )
    return(OS)
}
