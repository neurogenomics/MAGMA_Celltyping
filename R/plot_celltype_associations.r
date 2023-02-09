#' Plot celltype associations calculated using MAGMA
#'
#' Can take input from either
#'  \link[MAGMA.Celltyping]{calculate_celltype_associations} or
#'  \link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
#'
#' @param ctAssocs Output from either 
#'  \link[MAGMA.Celltyping]{calculate_celltype_associations} or
#'  \link[MAGMA.Celltyping]{calculate_conditional_celltype_associations}.
#' @param useSignificanceLine TRUE or FALSE. Should their be a vertical
#'  line marking bonferroni signifiance?
#' @param savePDF TRUE or FALSE. Save figure to file or print to screen?
#' @param fileTag String apprended to the names of the saved PDFs,
#'  i.e. the name of the celltype data file used
#' @param plotDendro Should the dendrogram of celltypes be
#'  shown alongside the figure? TRUE or FALSE.
#' @param gwas_title Title to be displayed over the figure (string).
#' @param plotLegend Should the figure legend be displayed?
#' @param figsDir Directory where figures should be created.
#' @param show_plot Print to the plot(s). 
#' @inheritParams calculate_celltype_associations
#'
#' @return A list of ggplot objects.
#'
#' @examples
#' res <- MAGMA.Celltyping::enrichment_results
#' ctAssocs <- res$`finn-a-AD.tsv.gz.35UP.10DOWN`$ctAssocsLinear
#' ctd <- ewceData::ctd()
#' figs <- MAGMA.Celltyping::plot_celltype_associations(
#'     ctAssocs = ctAssocs,
#'     ctd = ctd)
#' @export
#' @importFrom grDevices dev.off pdf 
#' @importFrom gridExtra grid.arrange
plot_celltype_associations <- function(ctAssocs,
                                       ctd, 
                                       useSignificanceLine = TRUE,
                                       savePDF = TRUE, 
                                       fileTag = "", 
                                       plotDendro = TRUE, 
                                       gwas_title = "", 
                                       plotLegend = TRUE, 
                                       figsDir = NA,
                                       show_plot = TRUE) {

    if(is.null(ctAssocs)){
        stop("ctAssocs is NULL.")
    }
    #### CHECK: THAT RESULTS FOR ONLY ONE GWAS WERE PROVIDED  ####
    # (for more than one use magma_tileplot.r)
    whichGWAS <- unique(gsub("DOWN\\..*", "DOWN", 
                             unique(ctAssocs[[1]]$results$GCOV_FILE)))
    if (length(whichGWAS) > 1) {
        stopper("Only results for one GWAS at a tile should be provided to",
                "plot_celltype_association. For multiple GWAS,",
                "use magma_tileplot().")
    }
    #### MAGMA paths #####
    magmaPaths <- get_magma_paths(
        gwas_sumstats_path = ctAssocs$gwas_sumstats_path, 
        upstream_kb = ctAssocs$upstream_kb, 
        downstream_kb = ctAssocs$downstream_kb)
    if (is.na(figsDir)) {
        figsDir <- magmaPaths$figs
    }
    if (!file.exists(figsDir)) {
        dir.create(figsDir)
    }
    #### CHECK: WAS A TITLE PROVIDED FOR THE PLOT? ####
    if (gwas_title == "") {
        gwas_title <- whichGWAS
    }

    # CHECK: THAT A MINIMAL SET OF COLUMN HEADERS 
    # ARE INCLUDED IN THE RESULTS TABLE
    requiredHeaders <- c("Celltype", "P", "log10p", "Method",
                         "EnrichmentMode", "CONTROL", "CONTROL_label")

    # Is the analysis top10%, linear or merged?
    # print(ctAssocs[[1]]$results)
    # print(unique(ctAssocs[[1]]$results$EnrichmentMode))
    if (length(unique(ctAssocs[[1]]$results$EnrichmentMode)) == 1) {
        if (unique(ctAssocs[[1]]$results$EnrichmentMode) == "Linear") {
            analysisType <- "Linear"
        } else {
            analysisType <- "TopDecile"
        }
    } else {
        analysisType <- "Merged"
    }

    # Generate the plots (for each annotation level seperately)
    ggplot2::theme_set(ggplot2::theme_bw())
    figures <- list()
    for (annotLevel in seq_len(length(grep("^level.", names(ctAssocs))))) {
        # SET: NEW COLUMN COMBINING METHODS or ENRICHMENT TYPES
        ctAssocs[[annotLevel]]$results$FullMethod <- 
            sprintf("%s %s", 
                    ctAssocs[[annotLevel]]$results$Method,
                    ctAssocs[[annotLevel]]$results$EnrichmentMode)

        if (plotDendro == TRUE) {
            # Order cells by dendrogram
            ctdDendro <- get_ctd_dendro(ctd, annotLevel = annotLevel)
            ctAssocs[[annotLevel]]$results$Celltype <-
                factor(ctAssocs[[annotLevel]]$results$Celltype,
                       levels = gsub(" |\\(|\\)", "\\.", 
                                     ctdDendro$ordered_cells))
        }

        a2 <- ggplot2::ggplot(ctAssocs[[annotLevel]]$results, 
                     ggplot2::aes_string(x = "factor(Celltype)", 
                                y = "-log10p",
                                fill = "FullMethod")) +
            # ggplot2::scale_y_reverse() +
            ggplot2::geom_bar(stat = "identity", position = "dodge") +
            ggplot2::coord_flip() +
            ggplot2::ylab(expression("-log"[10] * "(pvalue)")) +
            ggplot2::xlab("")
        a2 <- a2 +
            ggplot2::theme(legend.position = c(0.5, 0.8)) + 
            ggplot2::ggtitle(gwas_title) + 
            ggplot2::theme(legend.title = ggplot2::element_blank())
        if (plotLegend == FALSE) {
            a2 <- a2 + ggplot2::theme(legend.position = "none")
        }
        if (useSignificanceLine) {
            a2 <- a2 + 
                ggplot2::geom_hline(
                    yintercept = -log(
                        as.numeric(0.05 / 
                                       ctAssocs$total_baseline_tests_performed),
                        10), colour = "black")
        }
        theFig <- a2 + ggplot2::theme_bw()

        # If the results come from a BASELINE analysis...
        if (length(unique(ctAssocs[[1]]$results$CONTROL)) == 1) {
            if (plotDendro == TRUE) {
                theFig <- gridExtra::grid.arrange(
                    a2, ctdDendro$dendroPlot,
                   ncol = 2, 
                   widths = c(0.8, 0.2))
            }

            if (savePDF) {
                fName <- sprintf(
                    "%s/%s.%sUP.%sDOWN.annotLevel%s.Baseline.%s.%s.pdf", 
                    figsDir,
                    magmaPaths$gwasFileName,
                    ctAssocs$upstream_kb, 
                    ctAssocs$downstream_kb, 
                    annotLevel, 
                    fileTag, 
                    analysisType) 
                # print("here")
                height <-  1 + 2 * (dim(ctAssocs[[annotLevel]]$results)[1] / 10)
                grDevices::pdf(file = fName, 
                               width = 10, 
                               height = height)
                print(gridExtra::grid.arrange(
                    a2 + ggplot2::theme_bw(), 
                    ctdDendro$dendroPlot, 
                    ncol = 2, widths = c(0.8, 0.2)))
                grDevices::dev.off()
            } else {
                if(show_plot) print(theFig)
            }
            # IF THE RESULTS COME FROM A CONDITIONAL ANALYSIS
        } else {
            theFig <- theFig + ggplot2::facet_wrap(~CONTROL_label)

            if (savePDF) {
                fName <- sprintf(
                    "%s/%s.%sUP.%sDOWN.annotLevel%s.ConditionalFacets.%s.%s.pdf",
                    figsDir,
                    magmaPaths$gwasFileName, 
                    ctAssocs$upstream_kb, 
                    ctAssocs$downstream_kb, 
                    annotLevel, 
                    fileTag, 
                    analysisType)
                height <- 1 + 2 * (dim(ctAssocs[[annotLevel]]$results)[1] / 30)
                grDevices::pdf(file = fName, 
                               width = 25,
                               height = height)
                print(theFig + ggplot2::theme_bw())
                grDevices::dev.off()
            } else {
                if(show_plot) print(theFig)
            }
        }
        figures[[length(figures) + 1]] <- theFig
    }
    return(figures)
}
