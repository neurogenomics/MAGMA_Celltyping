magma_os_suffix <- function() {
    os_suffix <- switch(get_os(),
        Mac = "_mac",
        Windows = "_win",
        Linux = "",
        NULL = ""
    )
    return(os_suffix)
}
