magma_os_suffix <- function(os=get_os()) { 
    os_suffix <- switch(EXPR = os,
        Mac = "_mac",
        Windows = "_win",
        Linux = "",
        NULL = ""
    )
    return(os_suffix)
}
