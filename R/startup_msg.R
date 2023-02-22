startup_msg <- function(){
    volcano <- "\U0001F30B"
    cell <- "\U0001F9A0"
    vc <- paste(cell, volcano, cell, sep="")
    line <- function(n=5,txt="="){paste(rep(txt,n), collapse = "")}
    n <- 21
    paste0(
        line(n)," ",vc," Welcome to MAGMA.Celltyping ",vc," ",line(n),"\n",
        "This package uses MAGMA:\n",
        "https://ctg.cncr.nl/software/magma\n\n",
        "To cite MAGMA.Celltyping, please use:","\n",
        paste(stringr::str_wrap(
            paste(" *",
                  suppressWarnings(
                      utils::citation(
                          package = "MAGMA.Celltyping")$textVersion)
            ),
            exdent = 5,
            width = 80),
            collapse = "\n"),"\n",
        line(n)," ",vc," ",line(27)," ",vc," ",line(n),"\n"
    )
}
