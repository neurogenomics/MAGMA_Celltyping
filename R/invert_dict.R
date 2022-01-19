#' Invert dictionary
#' 
#' Swap the names/values from a named vector. 
#' 
#' @param dict Named vector. 
#' 
#' @keywords internal 
invert_dict <- function(dict){
    stats::setNames(names(dict), unname(dict))
}