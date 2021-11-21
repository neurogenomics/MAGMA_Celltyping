#' Plot heatmap of GWAS x cell-type enrichment results
#' 
#' Plot enrichment results from 
#' \link[MAGMA.Celltyping]{celltype_associations_pipeline}.
#'
#' @examples 
#' MAGMA_results <- MAGMA.Celltyping::enrichment_results
#' merged_results <- MAGMA.Celltyping::merge_results(res)
#' heat <- MAGMA.Celltyping::results_heatmap(
#'     merged_results = merged_results,
#'     fdr_thresh = 1)  
#' @export
#' @import ggplot2
results_heatmap <- function(merged_results,
                            title = NULL,
                            x_lab = NULL,
                            fdr_thresh = .05,
                            facet_formula = "EnrichmentMode ~ .",
                            x_var = "Celltype",
                            y_var = "GWAS",
                            fill_var = "-log1p(FDR)",
                            scales = "free_y",
                            space = "fixed",
                            show_plot = TRUE,
                            height = 5, 
                            width = 7,
                            dpi = 300,
                            save_path = file.path(
                                tempdir(),
                                "MAGMA_Celltyping.heatmap.jpg")
                            ) { 
    #### Check args #####
    if (!"GWAS" %in% colnames(merged_results)) {
        merged_results$GWAS <- merged_results$dataset
    }
    if (is.null(fdr_thresh)) fdr_thresh <- 1
    subtitle <- if (!is.null(fdr_thresh)) paste0("FDR < ", fdr_thresh) else NULL
    #### Filter by FDR ####
    plot_dat <- subset(merged_results, FDR < fdr_thresh)
    messager(formatC(nrow(plot_dat), big.mark = ","),
             "results @ FDR <",fdr_thresh)
    if(nrow(plot_dat)==0) stop("Filtered data must contain >0 rows.")
    #### Plot ####
    heat <- ggplot(
        data = plot_dat,
        aes_string(x = x_var, y = y_var, fill = fill_var)
    ) +
        geom_tile(color = "white") +
        facet_grid(
            facets = as.formula(facet_formula),
            scales = scales,
            space = space
        ) +
        labs(
            title = title,
            subtitle = subtitle,
            x = x_lab
        ) +
        scale_fill_gradient(low = "blue", high = "red", na.value = "white") +
        theme_bw() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
            strip.background = element_rect(fill = "grey20"),
            strip.text = element_text(color = "white"),
            panel.grid.minor = element_blank()
        )
    #### Print ####
    if (show_plot) print(heat)
    #### Save ####
    if (!is.null(save_path)){ 
        dir.create(dirname(save_path), showWarnings = FALSE, recursive = TRUE)
        ggplot2::ggsave(filename = save_path, 
                        plot = heat, 
                        dpi = dpi,
                        height = height, width = width)
    }
    return(heat)
}
