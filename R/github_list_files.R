github_list_files <- function(creator = "neurogenomics",
                              repo = "MAGMA_Files",
                              branch = c("main", "master"),
                              query = NULL,
                              return_download_api = T,
                              verbose = T) {
    repo_api <- file.path(
        "https://api.github.com/repos", creator, repo,
        paste0("git/trees/", branch[1], "?recursive=1")
    )
    req <- httr::GET(repo_api)
    httr::stop_for_status(req)
    filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
    printer(paste(length(filelist), "files found in GitHub repo:", file.path(creator, repo)), v = verbose)
    if (!is.null(query)) {
        # query_string <- "*Nalls23andMe_2019.*UKB.multi_finemap.csv.gz"
        bool <- grepl(query, filelist)
        filelist <- filelist[bool]
        printer(paste(length(filelist), "files found matching query."), v = verbose)
    }
    if (return_download_api) {
        filelist <- file.path("https://github.com", creator, repo, "raw", branch, filelist)
    }
    return(filelist)
}
