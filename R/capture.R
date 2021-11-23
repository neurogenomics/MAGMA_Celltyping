#' Capture output of function
#' 
#' Capture output of function and print to screen as a message.
#' This allows tables, summaries, etc to be printed to the console without 
#' messing up alignment of columns.
#' @param func_output Function output.
#' 
#' @return Null output.
#' @keywords internal
capture <- function(func_output){
    requireNamespace("utils")
    msg <- paste0(utils::capture.output(func_output), collapse = "\n")
    message(msg)
}