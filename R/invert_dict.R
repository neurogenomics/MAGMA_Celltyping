invert_dict <- function(dict){
    stats::setNames(names(dict), unname(dict))
}