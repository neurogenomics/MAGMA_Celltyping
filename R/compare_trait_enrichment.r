#' Test to see which cell types are more enriched in magmaPath2 than magmaPath1
#'
#' @param magmaPath1 A magma results file
#' @param magmaPath2 A magma results file
#' @param magma1 A magma results data structure
#' @param magma2 A magma results data structure
#' @param annotLevel Annotation level of the CTD / results file to use
#' @param ctd Cell type data structure containing $specificity_quantiles
#'
#' @return File path for the genes.out file
#'
#' @export compare_trait_enrichments
#' @importFrom data.table data.table
#' @importFrom stats pnorm
compare_trait_enrichments <- function(magmaPath1 = NA, 
                                      magmaPath2 = NA, 
                                      magma1 = NA,
                                      magma2 = NA, 
                                      annotLevel, ctd) {
    if (!is.na(magmaPath1) & !is.na(magmaPath2)) {
        magma1 <- load_magma_results_file(magmaPath1,
                                          annotLevel = annotLevel,
                                          ctd = ctd)
        magma2 <- load_magma_results_file(magmaPath2, 
                                          annotLevel = annotLevel, 
                                          ctd = ctd)
    }
    rownames(magma1) <- magma1$Celltype
    rownames(magma2) <- magma2$Celltype
    magma1 <- magma1[intersect(magma1$Celltype, magma2$Celltype), ]
    magma2 <- magma2[intersect(magma1$Celltype, magma2$Celltype), ]

    z <- (magma1$BETA - magma2$BETA) / sqrt(magma1$SE^2 + magma2$SE^2)
    pvalue2sided <- 2 * pnorm(-abs(z))
    magma2$z <- z
    magma2$comparisonP2sided <- pvalue2sided
    magma2$BETA_magma1 <- magma1$BETA
    magma2$BETA_magma2 <- magma2$BETA
    magma2$P_magma1 <- magma1$P
    magma2$P_magma2 <- magma2$P
    magma2$SE_magma1 <- magma1$SE
    magma2$SE_magma2 <- magma2$SE
    magma3 <- magma2[, c("Celltype", 
                         "BETA_magma1", "BETA_magma2",
                         "P_magma1", "P_magma2", 
                         "SE_magma1", "SE_magma2", 
                         "z", "comparisonP2sided")]
    magma3$moreSignifIn2 <- magma3$BETA_magma2 > magma3$BETA_magma1
    magma3$lessSignifIn2 <- magma3$BETA_magma2 < magma3$BETA_magma1
    p <- magma3$comparisonP2sided
    for (i in 1:length(p)) {
        if (magma3$z[i] < 0) {
            p[i] <- p[i] / 2
        } else {
            p[i] <- 1 - p[i] / 2
        }
    }
    magma3$comparisonP1sided_MoreEnrichedIn2 <- p
    magma3 <- magma3[order(magma3$comparisonP1sided_MoreEnrichedIn2), ]
    # magma2$moreSignif = moreSignif
    # magma2=magma2[order(magma2$comparisonP),]
    # magma2[magma2$P<0.05 & magma2$moreSignif==TRUE,]
    # print(magma3[magma3$comparisonP2sided<0.05,])
    return(magma3)
}


compare.trait.enrichments <- function(...){
    .Deprecated("compare_trait_enrichments")
    compare_trait_enrichments(...)
}
