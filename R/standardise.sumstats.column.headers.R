#' Standardise the column headers in the Summary Statistics files
#'
#' Use a reference data table of common column header names
#'  (stored as \link[MAGMA.Celltyping]{sumstatsColHeaders}) 
#'  convert them to a standard set, i.e. chromosome --> CHR.
#' This function does not check that all the required column headers
#'  are present.
#' The amended header is written directly back into the file.
#'
#' @param path File path for the summary statistics file.
#'
#' @returns The amended column headers 
#' (also the column headers will be written directly 
#' into the summary statistics file)
#'
#' @examples
#' \dontrun{ 
#' path <- MAGMA.Celltyping::get_example_gwas()
#' col_headers <- MAGMA.Celltyping::standardise.sumstats.column.headers(
#'     path = path)
#' }
#' @export
standardise.sumstats.column.headers <- function(path) {
    .Deprecated("MungeSumstats::format_sumstats")
    # Check the sumstats file exists
    if (!file.exists(path)) {
        stop("Path to GWAS sumstats is not valid")
    }
    # Read in the first line of the file only
    con <- file(path, "r")
    first_line <- readLines(con, n = 1)
    close(con)
    column_headers <- strsplit(first_line, "\t")[[1]]

    # Show to the user what the header is
    print("First line of summary statistics file: ")
    print(first_line)
    print(column_headers)

    # Amend the column headers based on a data table of commonly used names
    # data(sumstatsColHeaders)
    column_headers <- toupper(column_headers)
    for (headerI in 1:dim(MAGMA.Celltyping::sumstatsColHeaders)[1]) {
        un <- MAGMA.Celltyping::sumstatsColHeaders[headerI, 1]
        cr <- MAGMA.Celltyping::sumstatsColHeaders[headerI, 2]
        # print(un)
        if (un %in% column_headers & (!cr %in% column_headers)) {
            column_headers <- gsub(sprintf("^%s$", un), cr, column_headers)
        }
        # if(tolower(un) %in% column_headers){column_headers=gsub(sprintf("^%s$",tolower(un)),cr,column_headers)}
    }
    new_first_line <- paste(column_headers, collapse = "\t")

    # Write the new column headers to file
    sed_command <- sprintf("sed -i '' '1s/%s/%s/' %s", first_line, new_first_line, path)
    system2("/bin/bash", args = c("-c", shQuote(sed_command)))

    # column_headers = strsplit(column_headers," ")[[1]]
    return(column_headers)
}

 