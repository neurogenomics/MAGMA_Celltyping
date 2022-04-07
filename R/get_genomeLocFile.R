#' Import gene location files 
#'
#' Imports a file gene coordinate from its respective genome build.
#' 
#' @param build Genome build. Must be one of:
#' \itemize{
#' \item{"GRCH36" : }{Corresponds to file "NCBI36.3.gene.loc"}
#' \item{"GRCH37" : }{Corresponds to file "NCBI37.3.gene.loc"}
#' \item{"GRCH38" : }{Corresponds to file "NCBI38.gene.loc"}
#' }
#' @param storage_dir Folder in which to save the file.
#' @inheritParams piggyback::pb_download
#' 
#' @source
#' \code{
#' ## Move from data/ to Releases on GitHub to speed up MAGMA.Celltyping
#' prefixes <- c("NCBI36.3","NCBI37.3","NCBI38")
#' ### Made a permalink so we can always go back to this version of the data
#' ### (even after it's been deleted from the current version).
#' permalink_id <- "f931877dedcde103723f14242d5242fdca7b3af6"
#' files <- stats::setNames(
#'     paste("https://github.com/neurogenomics/MAGMA_Celltyping/raw",
#'               permalink_id,"data",
#'               paste0(prefixes,".gene.loc"), sep="/"),
#'     prefixes
#' )
#' local_files <- lapply(names(files), function(x){
#'     local <- file.path(tempdir(),basename(files[x]))
#'     utils::download.file(files[x],local)
#'     return(local)
#' }) %>% `names<-`(names(files))
#' 
#' for(x in names(local_files)){
#'     piggyback::pb_upload(file = local_files[[x]],
#'                          repo = "neurogenomics/MAGMA_Celltyping",
#'                          overwrite = TRUE)
#' }
#' }
#' @source
#' \code{
#' tmp <- MAGMA.Celltyping:::get_genomeLocFile(build = "GRCH37")
#' }
#' @return File path.
#' 
#' @keywords internal
#' @importFrom tools R_user_dir
#' @importFrom data.table fread
get_genomeLocFile <- function(build,
                              storage_dir = tools::R_user_dir(
                                  "MAGMA.Celltyping",
                                  which="cache"),
                              overwrite = FALSE) {
    build <- toupper(build[1])
    dict <- c(GRCH36 = "NCBI36.3.gene.loc",
              GRCH37 = "NCBI37.3.gene.loc",
              GRCH38 = "NCBI38.gene.loc")
    if(!build %in% names(dict)) {
        stopper("build must be one of:\n",
                paste("-",names(dict),collapse = "\n "))
    }
    selected_file <- dict[build]
    tmp <- get_data(
        fname = selected_file,
        storage_dir = storage_dir,
        overwrite = overwrite
    ) 
    #### Check that download was successful ####
    d <- data.table::fread(tmp, nrows = 10, nThread = 1, quote='')
    if(nrow(d)!=10 || ncol(d)!=6){
        stopper("genomeLocFile did not download properly.",
                "Please check that you have a stable internet connection, 
                restart your R session, and try again.")
    }
    return(tmp)
}
