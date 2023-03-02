check_n <- function(path_formatted,
                    N){
    
    if (is.null(N) || is.na(N)) {
        first_line <- readLines(path_formatted, n = 1)
        column_headers <- strsplit(first_line, "\t")[[1]]
        if ("N" %in% column_headers) {
            n_arg <- "ncol=N"
        } else {
            nval <- as.numeric(
                readline(paste(
                    "There is no N column within the sumstats file.",
                    "What is the N value for this GWAS?"
                ))
            )
            
            if (is.na(nval)) {
                stop(paste(
                    nval, "provided but value of N for",
                    "the GWAS must be numeric"
                ))
            }
            if (nval < 1000) {
                stop(paste(
                    "Value of N provided is less than 1,000.",
                    "This seems unlikely."
                ))
            }
            if (nval > 100000000) {
                stop(paste(
                    "Value of N provided is over than 100,000,000.",
                    "This seems unlikely."
                ))
            }
            n_arg <- sprintf("N=%s", nval)
        }
    } else {
        n_arg <- sprintf("N=%s", N)
    }
    return(n_arg)
}