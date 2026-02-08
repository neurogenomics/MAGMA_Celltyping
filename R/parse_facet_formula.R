#' Parse facet formula string to rows/cols for facet_grid
#'
#' Converts a formula string like "A ~ B" to the modern ggplot2 facet_grid
#' syntax using rows and cols arguments with vars().
#'
#' @param facet_formula A formula string like "A ~ .", ". ~ B", or "A ~ B".
#' @return A list with \code{rows} and \code{cols} elements, each either
#'   NULL or a vars() quosure.
#'
#' @examples
#' MAGMA.Celltyping:::parse_facet_formula("EnrichmentMode ~ .")
#' MAGMA.Celltyping:::parse_facet_formula(". ~ GWAS")
#' MAGMA.Celltyping:::parse_facet_formula("A ~ B")
#'
#' @keywords internal
parse_facet_formula <- function(facet_formula) {
    f <- stats::as.formula(facet_formula)

    # Extract LHS (rows) and RHS (cols) from formula
    lhs <- if (length(f) == 3) f[[2]] else NULL
    rhs <- if (length(f) == 3) f[[3]] else f[[2]]

    # Helper to convert expression to vars() quosure
    # Returns NULL if expression is "." (meaning no faceting on that dimension)
    to_vars <- function(expr) {
        if (is.null(expr) || identical(expr, as.symbol("."))) {
            return(NULL)
        }
        # expr is already a symbol or call, pass it directly to vars()
        eval(bquote(ggplot2::vars(.(expr))))
    }

    list(rows = to_vars(lhs), cols = to_vars(rhs))
}
