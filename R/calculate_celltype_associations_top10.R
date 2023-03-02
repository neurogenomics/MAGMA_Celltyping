calculate_celltype_associations_top10 <- function(magmaPaths,
                                                  ctd,
                                                  annotLevel,
                                                  ctd_species,
                                                  genesOutCOND,
                                                  sumstatsPrefix2,
                                                  analysis_name,
                                                  magma_version){
    # First match quantiles to the genes in the genes.out file...
    # then write as the genesCovar file (the input to MAGMA)
    geneCovarFile <- create_top10percent_genesets_file(
        genesOutFile = sprintf(
            "%s.genes.out",
            magmaPaths$filePathPrefix
        ),
        ctd = ctd,
        annotLevel = annotLevel,
        ctd_species = ctd_species
    )
    #### Run Top 10% mode (no conditioning) ####
    if (is.na(genesOutCOND[1])) {
        out_file <- paste0(sumstatsPrefix2,'.',
                           analysis_name)
        magma_cmd <- sprintf(
            paste(
                "magma",
                "--gene-results '%s.genes.raw'",
                "--set-annot '%s'",
                "--out '%s'"
            ),
            magmaPaths$filePathPrefix,
            geneCovarFile,
            out_file
        )
        #### Run Top 10% mode conditioned on another GWAS ####
    } else {
        geneCovarFile2 <- create_gene_covar_file(
            genesOutFile = sprintf(
                "%s.genes.out",
                magmaPaths$filePathPrefix
            ),
            ctd = ctd,
            annotLevel = annotLevel,
            ctd_species = ctd_species,
            genesOutCOND = genesOutCOND[1]
        )
        conditionOn <- paste(sprintf(
            "ZSTAT%s",
            seq_len(length(genesOutCOND[1]))
        ),
        collapse = ","
        )
        out_file <- paste0(sumstatsPrefix2,'.',
                           analysis_name)
        magma_cmd <- sprintf(
            paste(
                "magma",
                "--gene-results '%s.genes.raw'",
                if(magma_version>="1.07"){
                    paste(
                        "--set-annot '%s'",
                        "--gene-covar '%s'",
                        "--model direction=twosided condition='%s'"
                    )
                } else  { 
                    paste(
                        "--set-annot '%s' twosided",
                        "--gene-covar '%s' condition-only='%s'"
                    )
                },
                "--out '%s'"
            ),
            magmaPaths$filePathPrefix,
            geneCovarFile,
            geneCovarFile2,
            conditionOn,
            out_file
        )
    }
    return(
        list(magma_cmd=magma_cmd,
             geneCovarFile=geneCovarFile)
    )
}