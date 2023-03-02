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
#'  line marking Bonferroni significance?
#' @param savePDF TRUE or FALSE. Save figure to file or print to screen?
#' @param fileTag String appended to the names of the saved PDFs,
#'  i.e. the name of the celltype data file used
#' @param plotDendro Should the dendrogram of celltypes be
#'  shown alongside the figure? TRUE or FALSE.
#' @param gwas_title Title to be displayed over the figure (string).
#' @param plotLegend Should the figure legend be displayed?
#' @param figsDir Directory where figures should be created.
#' @param show_plot Print to the plot(s). 
#' @param bind_plots Bind the list of plots together using 
#' \link[patchwork]{wrap_plots}.
#' @param ncol Number of columns to put plots into during \code{bind_plots}.
#' @param verbose Print messages.
#' @inheritParams calculate_celltype_associations 
#' @return A list of ggplot objects.
#'
#' @export
#' @importFrom methods show
#' @importFrom stats setNames 
#' @examples 
#' ctAssocs <- MAGMA.Celltyping::enrichment_results[[1]]$ctAssocsLinear
#' ctd <- ewceData::ctd()
#' figs <- plot_celltype_associations(ctAssocs = ctAssocs,
#'                                    ctd = ctd)
plot_celltype_associations <- function(ctAssocs,
                                       ctd, 
                                       useSignificanceLine = TRUE,
                                       savePDF = TRUE, 
                                       fileTag = "", 
                                       plotDendro = TRUE, 
                                       gwas_title = "", 
                                       plotLegend = TRUE, 
                                       figsDir = NULL,
                                       show_plot = TRUE,
                                       bind_plots = FALSE,
                                       ncol = 1,
                                       verbose = TRUE) {
    # templateR:::args2vars(plot_celltype_associations)
    requireNamespace("ggplot2")
    requireNamespace("patchwork")
    
    if(is.null(ctAssocs)) stopper("ctAssocs is NULL.")
    #### Function to create PDF path ####
    pdf_path <- function(figsDir,
                         magmaPaths,
                         annotLevel,
                         fileTag,
                         analysisType,
                         type="Baseline"){
        file.path(figsDir, 
                   paste0(magmaPaths$gwasFileName,".",
                          ctAssocs$upstream_kb,"UP.",
                          ctAssocs$downstream_kb,"DOWN.",
                          "annotLevel",annotLevel,".",
                          type,".",
                          fileTag,".",
                          analysisType,'.',
                          "pdf"
                   )
        ) 
    }
    calc_height <- function(width,
                            ctAssocs,
                            annotLevel){ 
        1 + 2 * (dim(ctAssocs[[annotLevel]]$results)[1] / width)
    }
    #### CHECK: THAT RESULTS FOR ONLY ONE GWAS WERE PROVIDED  ####
    # (for more than one use magma_tileplot.r)
    whichGWAS <- unique(gsub("DOWN\\..*", "DOWN", 
                             unique(ctAssocs[[1]]$results$GCOV_FILE)))
    if (length(whichGWAS) > 1) {
        stopper("Only results for one GWAS at a tile should be provided to",
                "plot_celltype_association(). For multiple GWAS,",
                "use magma_tileplot().")
    }
    #### MAGMA paths #####
    magmaPaths <- get_magma_paths(
        gwas_sumstats_path = ctAssocs$gwas_sumstats_path, 
        upstream_kb = ctAssocs$upstream_kb, 
        downstream_kb = ctAssocs$downstream_kb)
    if (is.null(figsDir) || 
        is.na(figsDir)) {
        figsDir <- magmaPaths$figs
    }
    if (!file.exists(figsDir)) {
        dir.create(figsDir, showWarnings = FALSE, recursive = TRUE)
    }
    #### CHECK: WAS A TITLE PROVIDED FOR THE PLOT? ####
    if (gwas_title == "") gwas_title <- whichGWAS
    #### Is the analysis top10%, linear or merged? ####
    if (length(unique(ctAssocs[[1]]$results$EnrichmentMode)) == 1) {
        if (unique(ctAssocs[[1]]$results$EnrichmentMode) == "Linear") {
            analysisType <- "Linear"
        } else {
            analysisType <- "TopDecile"
        }
    } else {
        analysisType <- "Merged"
    } 
    #### Generate the plots (for each annotation level separately) ####
    ggplot2::theme_set(ggplot2::theme_bw()) 
    #### Handle both new and old enrichment result formats ####
    lvls <- seq_len(length(grep("^level.", names(ctAssocs))))
    if(length(lvls)==0){
        messager("No `level#` detected in results. Defaulting to level=1",
                 v=verbose)
        lvls <- 1
    }
    #### Iterate over results from each CTD level ####
    figures <- lapply(stats::setNames(lvls,
                                      paste0("level",lvls)),
                      function(annotLevel){ 
        messager(paste0("Plotting level ",annotLevel,":"),v=verbose)
        messager("+ Preparing data.",v=verbose)
        # SET: NEW COLUMN COMBINING METHODS or ENRICHMENT TYPES
        ctAssocs[[annotLevel]]$results$FullMethod <- 
            paste(ctAssocs[[annotLevel]]$results$Method,
                  ctAssocs[[annotLevel]]$results$EnrichmentMode)
        if (isTRUE(plotDendro)) {
            #### Order cells by dendrogram ####
            ctdDendro <- get_ctd_dendro(ctd = ctd, 
                                        annotLevel = annotLevel,
                                        expand = NULL, 
                                        verbose = verbose)
            ctAssocs[[annotLevel]]$results$Celltype <-
                factor(fix_celltype_names2(
                    celltypes = ctAssocs[[annotLevel]]$results$Celltype, 
                    make_unique = FALSE),
                       levels = fix_celltype_names2(
                           celltypes = ctdDendro$ordered_cells, 
                           make_unique = FALSE), 
                       ordered = TRUE)
        }
        #### Make the figure ####
        messager("+ Generating figure.",v=verbose)
        fig <- ggplot2::ggplot(ctAssocs[[annotLevel]]$results, 
                              ggplot2::aes_string(x = "factor(Celltype)", 
                                                  y = "-log10p",
                                                  fill = "FullMethod")) + 
            ggplot2::geom_bar(stat = "identity", 
                              position = "dodge") +
            ggplot2::coord_flip() +
            ggplot2::labs(x = NULL, 
                          y = expression("-log"[10] * "(pvalue)"),
                          title = gwas_title, 
                          legend = NULL) +
            ggplot2::theme(legend.position="top") 
        if (isFALSE(plotLegend)) {
            fig <- fig + ggplot2::theme(legend.position = "none", 
                                        legend.justification = c(0,0))
        }
        if (isTRUE(useSignificanceLine)) {
            fig <- fig + 
                ggplot2::geom_hline(
                    linetype="dotted",
                    yintercept = -log(
                        as.numeric(0.05 / 
                                       ctAssocs$total_baseline_tests_performed),
                        10), 
                    colour = "black", 
                    alpha=.8)
        } 
        #### Conditionnally format the plot ####
        #### If the results come from a BASELINE analysis ####
        if (length(unique(ctAssocs[[1]]$results$CONTROL)) == 1) {
            if (isTRUE(plotDendro)) {
                fig <- patchwork::wrap_plots(fig,  ctdDendro$dendroPlot) 
            } 
            width <- 10
            type <- "Baseline"
        
        #### IF THE RESULTS COME FROM A CONDITIONAL ANALYSIS ####
        } else {
            fig <- fig + ggplot2::facet_wrap(~CONTROL_label) 
            type <- "ConditionalFacets"
            width <- 30
        }
        #### Save plot ####
        if (isTRUE(savePDF)) { 
            fName <- pdf_path(figsDir = figsDir, 
                              magmaPaths = magmaPaths, 
                              annotLevel = annotLevel,
                              fileTag = fileTag, 
                              analysisType = analysisType, 
                              type = "ConditionalFacets")  
            messager("+ Saving figure ==>",fName,v=verbose)
            height <- calc_height(width = width,
                                  ctAssocs = ctAssocs, 
                                  annotLevel = annotLevel)
            dir.create(dirname(fName),showWarnings = FALSE, recursive = TRUE)
            ggplot2::ggsave(filename = fName,
                            plot = fig,
                            width = width, 
                            height = height)
        } 
        #### Show plot ####
        if(isTRUE(show_plot)) methods::show(fig)
        return(fig)
    }) 
    #### Return ####
    if(isTRUE(bind_plots)){
        pw <- 
            patchwork::wrap_plots(figures, 
                                  ncol = ncol) +
            patchwork::plot_annotation(tag_levels = list(names(figures)))
        return(pw)
    } else {
        return(figures)
    }
}
