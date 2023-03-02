ask_integer <- function(prompt) {
    # set up connection, default to stdin() if not set
    con <- getOption("ask_opts.con", stdin())
    # display prompt and options
    optlist <- "enter integer"
    prompt_opt <- paste0(prompt, " [", optlist, "]\n")
    cat(prompt_opt)
    input <- readLines(con = con, n = 1) 
    is_integer <- function(x){
        is.wholenumber <- function(x){as.numeric(x)%%1==0}
        !grepl("[^[:digit:]\\.-]",x) &&
            is.wholenumber(x)
    }
    if (!is_integer(input)) {
        cat("\U000274C Must provide an integer\n")
        input <- ask_integer(prompt=prompt)
    } else {
        cat("\U0002705 Valid integer detected.\n")
    }
    input
}
