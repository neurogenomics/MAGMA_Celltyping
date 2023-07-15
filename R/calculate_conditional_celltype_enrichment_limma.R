#' Calculate celltype enrichments without MAGMA
#'
#' Calculate celltype enrichments without MAGMA
#' using adjusted MAGMA Z-statistic from .genes.out files.
#'
#' @param magma1 Output from \link[MAGMA.Celltyping]{adjust_zstat_in_genesOut}
#'  (e.g. trait 1).
#' @param magma2 Output from \link[MAGMA.Celltyping]{adjust_zstat_in_genesOut}
#'  (e.g. trait 2).
#' @param show_plot Show plot visualizations of results (Default: \code{TRUE}).
#' @param ... Additional arguments passed to \link[EWCE]{standardise_ctd}.
#' @inheritParams calculate_celltype_associations
#' @inheritParams calculate_celltype_enrichment_limma
#'
#' @returns Data.frame with conditional enrichment results.
#'
#' @examples 
#' ctd <- ewceData::ctd()
#' 
#' #### MAGMA 1 #####
#' genesOut1 <- MAGMA.Celltyping::import_magma_files(
#'     ids = c("ieu-a-298"),
#'     file_types = ".genes.out",
#'     return_dir = FALSE)
#' magma1 <- MAGMA.Celltyping::adjust_zstat_in_genesOut(
#'     ctd = ctd, 
#'     magma_GenesOut_file = genesOut1)
#' 
#' #### MAGMA 2 #####
#' genesOut2 <- MAGMA.Celltyping::import_magma_files(
#'     ids = c("ukb-a-333"),
#'     file_types = ".genes.out",
#'     return_dir = FALSE)
#' magma2 <- MAGMA.Celltyping::adjust_zstat_in_genesOut(
#'     ctd = ctd, 
#'     magma_GenesOut_file = genesOut2)
#' 
#' #### Run conditional enrichment ####
#' cond_res <- 
#' MAGMA.Celltyping::calculate_conditional_celltype_enrichment_limma(
#'     magma1 = magma1,
#'     magma2 = magma2,
#'     ctd = ctd)
#' @export
#' @importFrom dplyr rename filter select
#' @importFrom stats residuals cor anova lm quantile
#' @importFrom reshape2 melt dcast
#' @importFrom lme4 lmer
calculate_conditional_celltype_enrichment_limma <- function(
    magma1,
    magma2,
    ctd,
    ctd_species = infer_ctd_species(ctd),
    annotLevel = 1,
    prepare_ctd = TRUE,
    thresh = 0.0001,
    show_plot = TRUE,
    verbose = TRUE,
    ...) { 
    
    #### Avoid confusing checks ####
    entrez <- hgnc_symbol <-  ADJ_ZSTAT <- ADJ_ZSTAT.y <- zNew <- NULL; 
    percentile <- ct <- log10p <- variable <- qs <- value <- NULL; 
    
    if(show_plot) requireNamespace("ggplot2")
    #### Join the two MAGMA Z-score data frames ####
    m1 <- magma1[!duplicated(magma1$entrez),]
    data.table::setkeyv(m1,"entrez")
    m2 <- magma2[!duplicated(magma2$entrez),] 
    data.table::setkeyv(m2,"entrez")
    shared <- intersect(m1$entrez, m2$entrez)
    m1a <- m1[as.character(shared), ] |> 
        dplyr::select(entrez,hgnc_symbol, ADJ_ZSTAT)
    m2a <- m2[as.character(shared), ] |> 
        dplyr::select(entrez,hgnc_symbol, ADJ_ZSTAT)
    m3 <- merge(m1a, m2a, by = c("entrez","hgnc_symbol"))
    #### Regress the second from the first & get the residuals ####
    messager("Correlation between magma1 and magma2 ADJ_ZSTAT:",
             round(stats::cor(m3$ADJ_ZSTAT.x, m3$ADJ_ZSTAT.y),3),
             v=verbose)
    mod <- stats::lm(data = m3, ADJ_ZSTAT.x ~ ADJ_ZSTAT.y)
    # capture(stats::anova(mod)) 
    capture(summary(mod))
    m3$zNew <- stats::residuals(mod)
    m3 <- m3 |>
        dplyr::rename(ADJ_original = ADJ_ZSTAT.y) |>
        dplyr::rename(ADJ_ZSTAT.y = zNew)
    magma2_NEW <- merge(x = m3[, c("entrez", "ADJ_ZSTAT.y")] |> 
                            dplyr::rename(ADJ_ZSTAT = ADJ_ZSTAT.y), 
                        y = magma2 |>
                            dplyr::select(entrez, hgnc_symbol),
                        by = "entrez")
    #### Melt it (so it's ready for mixed modelling)! ####
    m4 <- reshape2::melt(m3, id.vars = c("entrez","hgnc_symbol"))
    m5 <- merge(x = m4, 
                y = m1[, c("entrez", "hgnc_symbol")], 
                by = c("entrez","hgnc_symbol"))
    magmaAdjZ <- m5
    #### Standardise CTD ####
    if(prepare_ctd){
        output_species <- "human"
        ctd <- EWCE::standardise_ctd(ctd = ctd,
                                     dataset = "CTD",
                                     input_species = ctd_species,
                                     output_species = output_species,
                                     verbose = verbose)
        ctd_species <- output_species
    }
    #### First get names of all cell types ###
    allCellTypes <- colnames(ctd[[annotLevel]]$specificity)
    #### Initialise variables ####
    ps <- coef <- rep(0, length(allCellTypes))
    names(ps) <- names(coef) <- allCellTypes
    #### Loop over each celltype testing for enrichment ####
    count <- 0
    for (ct1 in allCellTypes) {
        count <- count + 1
        messager("Running conditonal enrichment tests:",ct1, v=verbose)
        hgncS <- magmaAdjZ$hgnc_symbol
        props <- ctd[[annotLevel]]$specificity[hgncS, ct1]
        notExp <- rep(0, length(props))
        #### Drop any genes with expression below threshold ####
        hgncS <- hgncS[props > thresh]
        props <- props[props > thresh]
        notExp[props < thresh] <- -1
        #### Determine which expression decile genes fall into ####
        quantiles <- stats::quantile(props[props > thresh],
                                     probs = seq(from = 0, to = 1, by = 0.1))
        perc <- as.numeric(cut(props, quantiles, include.lowest = TRUE))
        perc[is.na(perc)] <- 0
        #### Merge decile groups with MAGMA zscores ####
        geneGroups <- data.frame(
            hgnc_symbol = hgncS,
            proportion = props,
            percentile = perc,
            stringsAsFactors = FALSE,
            check.rows = FALSE,
            check.names = FALSE
        )
        magma_with_ct1 <- geneGroups |> 
            merge(magmaAdjZ, by = "hgnc_symbol") |>
            dplyr::filter(percentile >= 0)
        #### Fit a linear model and get p,-value and coefficient (slope) ####
        mZ <- reshape2::dcast(
            data = magma_with_ct1,
            formula = hgnc_symbol ~ variable,
            fun.aggregate = mean
        )  
        mZ2 <- merge(
            x = mZ,
            y = magma_with_ct1[, c("hgnc_symbol", "percentile")],
            by = "hgnc_symbol"
        ) |> unique()
        mZ2$residualPercentile <- stats::residuals(
            stats::lm(formula = percentile ~ ADJ_ZSTAT.y,
                      data = mZ2)
        )
        #### Run lmer ####
        mod.mod <- lme4::lmer(value ~ 1 + variable + percentile + 
                                  variable * percentile + 
                                  (1 | hgnc_symbol), 
                              data = unique(magma_with_ct1),
                              REML = FALSE)
        mod.null <- lme4::lmer(value ~ 1 + 
                                   variable + percentile + 
                                   (1 | hgnc_symbol),
                               data = unique(magma_with_ct1),
                               REML = FALSE)
        modANOVA <- stats::anova(mod.mod, mod.null)
        #### Convert p-value to one-sided ####
        ps[count] <- modANOVA$`Pr(>Chisq)`[2]
        coef[count] <- summary(mod.mod)$coefficients[
            "variableADJ_ZSTAT.y:percentile", 1]
    }
    #### Get baseline results ####
    baseline1 <- calculate_celltype_enrichment_limma(
        magmaAdjZ = magma1, 
        ctd = ctd, 
        thresh = thresh, 
        ctd_species = ctd_species, 
        annotLevel = annotLevel
    )
    baseline2 <- calculate_celltype_enrichment_limma(
        magmaAdjZ = magma2_NEW, 
        ctd = ctd,
        thresh = thresh, 
        ctd_species = ctd_species, 
        annotLevel = annotLevel)
    baselineRes <- data.frame(ct = names(baseline1), 
                              p1_baseline = baseline1, 
                              p2_baseline = baseline2)
    output <- list(ps = ps, coef = coef)
    df <- data.frame(ct = names(output$ps), 
                     ps = output$ps, 
                     coef = output$coef)
    df2 <- merge(df, baselineRes, by = "ct")
    df2 <- df2[order(df2$coef), ]
    df2$qs <- p.adjust(df2$ps, method = "BH")
    df3 <- reshape2::melt(df2, id.vars = c("ct", "coef", "ps", "qs"))
    df3$direction <- "No Change"
    df3$direction[df3$coef > 0 & df3$qs < 0.05] <- "Increased Enrichment"
    df3$direction[df3$coef < 0 & df3$qs < 0.05] <- "Decreased Enrichment"
    df3$log10p <- log10(df3$value)
    #### Plot 2 ####
    if(show_plot){ 
        plot1 <- ggplot2::ggplot(df3) +
            ggplot2::geom_bar(
                ggplot2::aes(x = ct, y = log10p, fill = variable),
                stat = "identity", position = "dodge") +
            ggplot2::scale_y_reverse() +
            ggplot2::facet_wrap(~direction) +
            ggplot2::coord_flip() +
            ggplot2::ylab(expression("-log"[10] * "(pvalue)")) +
            ggplot2::xlab("") +
            ggplot2::theme_bw()
        print(plot1)
    } 
    # Now plot the baseline in one facet + significance of changes in the next
    df3b <- df3 
    df4_a <- df3b[, c("ct", "qs", "coef")]
    df4_a$log10p <- log(df4_a$qs)
    df4_a[df4_a$coef < 0, ]$log10p <- -1 * df4_a[df4_a$coef < 0, ]$log10p
    df4_a <- df4_a |> dplyr::select(ct, qs, log10p)
    df4_a$variable <- ""
    df4_a$type <- "Significance of Changes"
    df4_b <- df3[, c("ct", "value", "log10p", "variable")] |> 
        dplyr::rename(qs = value)
    df4_b$type <- "Baseline"
    df4 <- rbind(df4_a, df4_b)
    #### Plot 2 ####
    if(show_plot){
        plot2 <- ggplot2::ggplot(df4) +
            ggplot2::geom_bar(
                ggplot2::aes(x = ct, y = log10p, fill = variable), 
                stat = "identity", position = "dodge") +
            ggplot2::scale_y_reverse() +
            ggplot2::facet_wrap(~type, scale = "free_x") +
            ggplot2::coord_flip() +
            ggplot2::ylab(expression("-log"[10] * "(pvalue)")) +
            ggplot2::xlab("") +
            ggplot2::theme_bw()
        methods::show(plot2)
    }
    return(df2 |> dplyr::rename(Celltype = ct))
}

calculate.conditional.celltype.enrichment.probabilities.wtLimma <-
    function(...) {
        .Deprecated(
            "calculate_conditional_celltype_enrichment_limma"
        )
        calculate_conditional_celltype_enrichment_limma(...)
}
