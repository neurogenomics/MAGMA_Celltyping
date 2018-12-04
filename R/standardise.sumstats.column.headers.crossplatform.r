standardise.sumstats.column.headers.crossplatform <- function (first_line) 
{
  column_headers = strsplit(first_line, "\t")[[1]]
  print("First line of summary statistics file: ")
  print(first_line)
  print(column_headers)
  data(sumstatsColHeaders) # Loads the pre-defined "common column names" for GWAS data
  
  column_headers = toupper(column_headers)
  print(column_headers)
  
  for (headerI in 1:dim(sumstatsColHeaders)[1]) {
    un = sumstatsColHeaders[headerI, 1]
    cr = sumstatsColHeaders[headerI, 2]
    if (un %in% column_headers & (!cr %in% column_headers)) {
      column_headers = gsub(sprintf("^%s$", un), cr, column_headers)
    }
  }
  new_first_line = paste(column_headers, collapse = "\t")
  return(new_first_line)
}
