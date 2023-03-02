calculate_celltype_associations_linear <- function(magmaPaths,
                                                   ctd,
                                                   annotLevel, 
                                                   ctd_species,
                                                   genesOutCOND,
                                                   sumstatsPrefix2,
                                                   analysis_name){
    
    # First match quantiles to the genes in the genes.out file...
    # then write as the genesCovar file (the input to MAGMA) 
    geneCovarFile <- create_gene_covar_file(
        genesOutFile = sprintf(
            "%s.genes.out",
            magmaPaths$filePathPrefix
        ),
        ctd = ctd,
        annotLevel = annotLevel,
        ctd_species = ctd_species,
        genesOutCOND = genesOutCOND
    )
    #### Run Linear mode (no conditioning) ####
    if (is.na(genesOutCOND[1])) {
        out_file <- paste0(sumstatsPrefix2,'.',
                           analysis_name)
        magma_cmd <- sprintf(
            paste(
                "magma",
                "--gene-results '%s.genes.raw'",
                "--gene-covar '%s'",
                "--model direction=pos --out '%s'"
            ),
            magmaPaths$filePathPrefix,
            geneCovarFile,
            out_file
        )
        #### Run Linear mode conditioned on another GWAS ####
    } else {
        conditionOn <- paste(sprintf(
            "ZSTAT%s",
            seq_len(length(genesOutCOND))
        ),
        collapse = ","
        )
        out_file <- paste0(sumstatsPrefix2,'.',
                           analysis_name)
        magma_cmd <- sprintf(
            paste(
                "magma",
                "--gene-results '%s.genes.raw'",
                "--gene-covar '%s'",
                "--model direction=pos  condition-residualize='%s'",
                "--out '%s'"
            ),
            magmaPaths$filePathPrefix,
            geneCovarFile,
            conditionOn,
            out_file
        )
    }
    return(
        list(magma_cmd=magma_cmd,
             geneCovarFile=geneCovarFile)
    )
}