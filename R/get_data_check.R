get_data_check <- function(tmp){
    l <- readLines(tmp)
    if(any(grepl("Bad credentials",l, ignore.case = TRUE))){
        file.remove(tmp)
        stp <- paste(
            "piggyback::pb_download() failed due to bad GitHub credentials.",
            "Please add a GitHub Personal Access Token (PAT)",
            "to your ~/.Renviron file and restart R",
            "before retrying this function.\ne.g.:\n\n",
            "   GITHUB_TOKEN=<your_PAT_here>",
            "\n\nIf you do not yet have a GitHub PAT,",
            "please follow instructions here:\n\n   ",
            paste("https://docs.github.com/en/authentication",
            "keeping-your-account-and-data-secure",
            "creating-a-personal-access-token",sep="/")
            )
        stop(stp)
    }
}
