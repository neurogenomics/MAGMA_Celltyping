#' List files
#' 
#' List files in a GitHub repository.
#' 
#' @param user Repository user name.
#' @param repo Repository name.
#' @param branch Which branch to search.
#' @param query A substring search to filter files with.
#' @param return_download_api Return the URL to download the file 
#' via GitHub's API, as opposed to the URL to view the file in a web browser.
#' @param verbose Print messages.
#' 
#' @return List of URLs.
#' 
#' @keywords internal
#' @importFrom httr GET stop_for_status content
github_list_files <- function(user = "neurogenomics",
                              repo = "MAGMA_Files_Public",
                              branch = c("master", "main"),
                              query = NULL,
                              return_download_api = TRUE,
                              verbose = TRUE) {
    requireNamespace("httr")
    
    repo_api <- file.path(
        "https://api.github.com/repos", user, repo,
        paste0("git/trees/", branch[1], "?recursive=1")
    )
    req <- httr::GET(repo_api)
    httr::stop_for_status(req)
    filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), 
                       use.names = FALSE)
    messager(paste(length(filelist), "files found in GitHub repo:",
                   file.path(user, repo)), v = verbose)
    if (!is.null(query)) {
        # query_string <- "*Nalls23andMe_2019.*UKB.multi_finemap.csv.gz"
        bool <- grepl(query, filelist)
        filelist <- filelist[bool]
        messager(paste(length(filelist), "files found matching query."),
                 v = verbose)
    }
    if (return_download_api) {
        filelist <- file.path("https://github.com", user, repo, "raw", 
                              branch, filelist)
    }
    return(filelist)
}
