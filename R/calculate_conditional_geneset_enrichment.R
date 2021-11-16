#' Use MAGMA to test enrichment in a geneset
#'
#' Assumes that you have already run \link[MAGMA.Celltyping]{map_snps_to_genes}.
#'
#' @param geneset Genes which are to be tested (as HGNC / MGI symbols).
#' @param geneset_species Species name relevant to the genes in the geneset.
#' @param controlledAnnotLevel Annotation level of the celltypes
#' being controlled for.
#' @param controlledCTs Array of the celltype to be controlled for,
#' e.g. c("Interneuron type 16","Medium Spiny Neuron).
#' @inheritParams calculate_celltype_associations
#'
#' @return File path for the genes.out file
#'
#' @export
#' @importFrom data.table data.table
#' @importFrom stats pnorm
calculate_conditional_geneset_enrichment <- function(geneset,
                                                     ctd,
                                                     controlledAnnotLevel = 1,
                                                     controlledCTs,
                                                     gwas_sumstats_path,
                                                     analysis_name,
                                                     upstream_kb = 35,
                                                     downstream_kb = 10,
                                                     genome_ref_path,
                                                     geneset_species = "mouse",
                                                     sctSpecies = "mouse") {
    gwas_sumstats_path <- path.expand(gwas_sumstats_path)
    magmaPaths <- get_magma_paths(
        gwas_sumstats_path = gwas_sumstats_path,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb
    )

    # First, check that the genes are HGNC/MGI IDs
    if (geneset_species == "human") {
        if (sum(geneset %in% all_hgnc_wtEntrez$hgnc_symbol) < 0.5) {
            stop("Less than 50% of the geneset are recognised HGNC symbols.
                    Have you entered them in the wrong format?
                    Or wrong species?")
        }
        ### all_hgnc_wtEntrez is a built-in dataset
        geneset_entrez <- all_hgnc_wtEntrez[
            all_hgnc_wtEntrez$hgnc_symbol %in% geneset,
        ]$entrezgene
    } else if (geneset_species == "mouse") {
        if (sum(geneset %in% One2One::ortholog_data_Mouse_Human$orthologs_one2one$mouse.symbol) < 0.25) {
            stop(
                "Less than 25% of the geneset are recognised
            MGI symbols with 1:1 orthologs.
            Have you entered them in the wrong format? Or wrong species?"
            )
        }
        geneset_m2h <- One2One::ortholog_data_Mouse_Human$orthologs_one2one[One2One::ortholog_data_Mouse_Human$orthologs_one2one$mouse.symbol %in% geneset, ]$human.symbol
        geneset_entrez <- MAGMA.Celltyping::all_hgnc_wtEntrez[MAGMA.Celltyping::all_hgnc_wtEntrez$hgnc_symbol %in% geneset_m2h, ]$entrezgene
    }

    # Check for errors in arguments
    check_inputs_to_magma_celltype_analysis(
        ctd = ctd,
        gwas_sumstats_path = gwas_sumstats_path,
        analysis_name = analysis_name,
        upstream_kb = upstream_kb,
        downstream_kb = downstream_kb,
        genome_ref_path = genome_ref_path
    )

    # Check the cell type 'controlledCT' exists at the relevant annotation level
    for (i in seq_len(length(controlledCTs))) {
        annotLvlCTs <- colnames(ctd[[
        as.numeric(controlledAnnotLevel)]]$specificity)
        reqCT <- controlledCTs[[i]]
        if (!reqCT %in% annotLvlCTs) {
            stop(sprintf(
                "User requested the following cell type be controlled
                         for but it cannot be found:
                         %s from annotation level %s",
                reqCT, controlledAnnotLevel
            ))
        }
    }

    # Write cell type specificity to disk (so it can be read by MAGMA)
    quantDat2 <- map_specificity_to_entrez(
        ctd = ctd,
        annotLevel = controlledAnnotLevel,
        sctSpecies = sctSpecies
    )
    geneCovarFile <- tempfile()
    write.table(
        x = quantDat2,
        file = geneCovarFile,
        quote = FALSE,
        row.names = FALSE,
        sep = "\t"
    )
    ctrldCTs <- gsub(" ", ".", paste(controlledCTs, collapse = ","))

    #### Drop any genes from geneset which do not have matching entrez in ctd
    geneset_entrez2 <- geneset_entrez[geneset_entrez %in% quantDat2$entrez]
    ctRows <- paste(c(analysis_name, geneset_entrez2), collapse = " ")

    ##### Write geneset file to disk (so it can be read by MAGMA) ####
    geneSetFile <- tempfile()
    write.table(
        x = ctRows,
        file = geneSetFile,
        quote = FALSE,
        row.names = FALSE,
        sep = "\t",
        col.names = FALSE
    )

    #### Run conditional analysis ####
    magma_cmd_cond <- sprintf(
        "magma",
        "--gene-results '%s.genes.raw'",
        "--set-annot '%s'",
        "--gene-covar '%s'",
        "--model direction=positive condition='%s'",
        "--out '%s.%s'",
        magmaPaths$filePathPrefix,
        geneSetFile,
        geneCovarFile,
        ctrldCTs,
        magmaPaths$filePathPrefix,
        analysis_name
    )
    messager(magma_cmd_cond)
    system(magma_cmd_cond)

    path <- sprintf("%s.%s.sets.out", magmaPaths$filePathPrefix, analysis_name)
    res <- read.table(path, stringsAsFactors = FALSE)
    colnames(res) <- as.character(res[1, ])
    res <- res[-1, ]

    path <- sprintf("%s.%s.gsa.out", magmaPaths$filePathPrefix, analysis_name)
    res_cond <- read.table(path, stringsAsFactors = FALSE)
    colnames(res_cond) <- as.character(res_cond[1, ])
    res_cond <- res_cond[-1, ]
    res_cond <- res_cond[res_cond$VARIABLE == analysis_name, ]

    # Calculate significance of difference
    # between baseline and conditional analyses
    z <- (as.numeric(res$BETA) -
        as.numeric(res_cond$BETA)) /
        sqrt(as.numeric(res$SE)^2 + as.numeric(res_cond$SE)^2)
    p <- 2 * stats::pnorm(-abs(z))

    # Convert to one sided probability
    # (that the conditional analysis is LESS significant than the baseline)
    if (res$BETA > res_cond$BETA) {
        pOneSided <- p / 2
    } else {
        pOneSided <- 1 - p / 2
    }
    #### Gather results into data.table ####
    full_res <- data.table::data.table(
        SET = res$SET,
        NGENES = res$NGENES,
        BASELINE_BETA = res$BETA,
        BASELINE_BETA_STD = res$BETA_STD,
        BASELINE_SE = res$SE,
        BASELINE_P = res$P,
        COND_BETA = res_cond$BETA,
        COND_BETA_STD = res_cond$BETA_STD,
        COND_SE = res_cond$SE,
        COND_P = res_cond$P,
        conditionedCTs = ctrldCTs,
        z = z,
        p_twoSided = p,
        p_oneSided = pOneSided
    )
    return(full_res)
}
