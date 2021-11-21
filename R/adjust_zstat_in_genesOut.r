#' Adjust MAGMA Z-statistic from .genes.out files
#'
#' Used when you want to directly analyse the gene level
#'  z-scores corrected for gene length etc
#'
#' @param magma_GenesOut_file A MAGMA .genes.out file.
#' @param ... Additional arguments passed for \link[EWCE]{standardise_ctd}. 
#' @inheritParams calculate_celltype_associations
#' @inheritParams EWCE::standardise_ctd
#' @inheritParams stats::p.adjust
#'
#' @examples
#' myGenesOut <- MAGMA.Celltyping::import_magma_files(
#'     ids = c("ieu-a-298"),
#'     file_types = ".genes.out",
#'     return_dir = FALSE)
#' ctd <- ewceData::ctd()
#' 
#' magmaGenesOut <- MAGMA.Celltyping::adjust_zstat_in_genesOut(
#'     ctd = ctd,
#'     magma_GenesOut_file = myGenesOut,
#'     ctd_species = "mouse"
#' )
#' @export
#' @importFrom stats lm p.adjust 
#' @importFrom utils read.table
adjust_zstat_in_genesOut <- function(ctd,
                                     magma_GenesOut_file = NA,
                                     ctd_species = "mouse",
                                     output_species = "human",
                                     method = "bonferroni",
                                     ...) {
    
    #### Convert orthologs to human gene symbols ####
    if (ctd_species != output_species) { 
        ctd <- EWCE::standardise_ctd(ctd = ctd,
                                     dataset = "NULL",
                                     input_species = ctd_species, 
                                     output_species = output_species,
                                     ...)
    } 
    allGenes <- unname(rownames(ctd[[1]]$specificity))
    #### Load the MAGMA data ####
    magma <- utils::read.table(magma_GenesOut_file,
        stringsAsFactors = FALSE,
        header = TRUE,
        check.names = FALSE
    )
    magma$entrez <- magma$GENE
    magma <- merge(
        x = magma,
        y = MAGMA.Celltyping::hgnc2entrez_orthogene,
        by = "entrez"
    )
    #### Filter MAGMA results ####
    magma <- magma[order(magma$P), ]  
    magma <- magma[magma$hgnc_symbol %in% allGenes, ]
    magma <- magma[!duplicated(magma$hgnc_symbol), ]
    #### Apply multiple testing correction ####
    magma$Q <- stats::p.adjust(magma$P, method = method)
    magma$logNSNPS <- log(magma$NSNPS)
    magma$logNPARAM <- log(magma$NPARAM)
    #### Remove MHC region ####
    magma <- magma[!(magma$CHR == 6 &
                         magma$START >= 25000000 &
                         magma$STOP <= 34000000), ] # DROP MHC: chr6, 25-34 mb
    magma$GENELEN <- abs(magma$STOP - magma$START)
    magma$logGENELEN <- log(magma$GENELEN)

    # Regress out effects of NSNPS and NPARAM (see 'boxplots_by_decile.r'
    # and the section on downsampling for info)
    #--- NSNPS only really has mjaor effects (i.e. zscore+2) when
    # a gene has ~10000 SNPS
    mod <- stats::lm(ZSTAT ~ 
                         NSNPS + logNSNPS + 
                         NPARAM + logNPARAM + 
                         GENELEN + logGENELEN,
                     data = magma)
    magma$ADJ_ZSTAT <- magma$ZSTAT - (
        magma$NSNPS * mod$coefficients[2] +
        magma$logNSNPS * mod$coefficients[3] +
        magma$NPARAM * mod$coefficients[4] + 
        magma$logNPARAM * mod$coefficients[5] +
        magma$GENELEN * mod$coefficients[6] + 
        magma$logGENELEN * mod$coefficients[7])
    # magma = arrange(magma,desc(magma$ADJ_ZSTAT))
    # magma = magma[order(magma$ADJ_ZSTAT,decreasing=TRUE),]
    return(magma)
}


adjust.zstat.in.genesOut <- function(...) {
    .Deprecated(adjust_zstat_in_genesOut)
    adjust_zstat_in_genesOut(...)
}
