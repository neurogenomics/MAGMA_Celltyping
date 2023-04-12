#' Get remote data
#'
#' Download remotely stored data via \link[piggyback]{pb_download}.
#' @keywords internal
#' @inheritParams piggyback::pb_download
#' @importFrom tools R_user_dir
#' @importFrom gh gh_token
get_data <- function(fname,
                     repo = "neurogenomics/MAGMA_Celltyping",
                     tag = "v2.0.8",
                     .token = gh::gh_token(),
                     storage_dir = tools::R_user_dir(
                       package = "MAGMA.Celltyping",
                       which = "cache"
                     ),
                     overwrite = TRUE,
                     check = FALSE
                     ){
  tmp <- fix_path(file.path(storage_dir, fname))
  requireNamespace("piggyback")
  requireNamespace("gh")
  Sys.setenv("piggyback_cache_duration" = 10)
  dir.create(storage_dir, showWarnings = FALSE, recursive = TRUE) 
  if(as.character(.token)=="")  {
    stopper("GitHub Personal Access Token (PAT) is missing.",
            "This is required to download Release files via piggyback.",
            "See GitHub for details:",
            paste0("https://docs.github.com/en/authentication/",
                   "keeping-your-account-and-data-secure/",
                   "creating-a-personal-access-token"))  
    } else {
      piggyback::pb_download(
        file = fname,
        tag = tag,
        dest = storage_dir,
        repo = repo,
        overwrite = overwrite,
        .token = .token
      )
    }

  #### Check that download didn't fail due to bad credentials #####
  if(isTRUE(check)){
    get_data_check(tmp = tmp)
  }
  return(tmp)
}


