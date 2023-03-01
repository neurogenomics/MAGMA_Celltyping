#' Gather MAGMA links
#' 
#' Gather links to MAGMA executables stored in the official MAGMA archives, 
#' and format them as a table. 
#' 
#' @keywords internal  
magma_links_gather <- function(){
    
    link <- NULL;
    links <- magma_links_query(latest_only = FALSE)
    #### Make new col with version number ####
    versions <- magma_links_versions(links = links,
                                     return_all = TRUE,
                                     unique_only = FALSE,
                                     filter_v = FALSE)
    meta <- data.table::data.table(link=links,
                                   version=versions) |>
        dplyr::arrange(dplyr::desc(version))
    #### Add column indicating which rows are the latest version #### 
    meta$latest <- meta$version==rev(versions)[1]
    #### Get OS ####
    suff_dict <- stats::setNames(
        c(paste0(magma_os_suffix(os = "Mac"),".zip"),
          paste0(magma_os_suffix(os = "Windows"),".zip"),
          paste0(magma_os_suffix(os = "Linux"),".zip")
        ),
        c("Mac","Windows","Linux")
    )  
    meta <- meta |> 
        dplyr::mutate(os=
            ifelse(
                grepl("_source|static|icc|gpp", link), "source", 
                   ifelse(
                       grepl(suff_dict["Mac"], link), "Mac",
                          ifelse(
                              grepl(suff_dict["Windows"], link), "Windows",
                                 ifelse(
                                     grepl(suff_dict["Linux"], link),
                                     "Linux",NA)
                              )
                       )
                ) 
            )  
    #### Give each file a name ####
    meta <- cbind(name = gsub(".zip","",basename(meta$link)), 
                  meta)
    return(meta)
}
