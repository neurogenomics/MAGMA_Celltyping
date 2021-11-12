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
