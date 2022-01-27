#' Create data table
#' 
#' Create an interactive data table with download buttons.
#' 
#' @param scrollY Max height (in pixels) before wrapping the 
#' table in a vertical scroll bar.
#' @inheritParams DT::datatable 
#' 
#' @returns datatables object.
#' 
#' @export
#' @examples 
#' data("mtcars")
#' create_DT(mtcars)
create_DT <- function(data,
                      caption = "",
                      scrollY = 400) {
    requireNamespace("DT")
    DT::datatable(
        data = data,
        caption = caption, extensions = "Buttons",
        options = list(
            dom = "Bfrtip", buttons = c(
                "copy", "csv",
                "excel", "pdf", "print"
            ), scrollY = scrollY, scrollX = TRUE,
            scrollCollapse = TRUE, paging = FALSE, 
            columnDefs = list(list(
                className = "dt-center",
                targets = "_all"
            ))
        )
    ) 
}
