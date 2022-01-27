#' Check enrichment mode
#' 
#' For functions where \code{EnrichmentMode} is an argument, 
#' check whether a valid option has been selected. 
#' 
#' @keywords internal
check_enrichment_mode <- function(EnrichmentMode) {
    if (!EnrichmentMode %in% c("Linear", "Top 10%")) {
        stop_msg <- paste0(
            "EnrichmentMode argument must be one of:",
            "'Linear' or 'Top 10%'."
        )
        stop(stop_msg)
    }
}
