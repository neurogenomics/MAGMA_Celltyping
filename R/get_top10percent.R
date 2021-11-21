get_top10percent <- function(ctd,
                             annotLevel,
                             ctd_species) {
    ctd2 <- map_specificity_to_entrez(
        ctd = ctd,
        annotLevel = annotLevel,
        ctd_species = ctd_species,
        return_ctd = TRUE
    )
    ctd3 <- prepare_quantile_groups(
        ctd = ctd2,
        ## must set standardise=FALSE
        ## or else new "quantDat2" matrix will be dropped.
        standardise = FALSE,
        input_species = ctd_species,
        output_species = "human",
        numberOfBins = 10
    )
    quantDat2 <- ctd3[[annotLevel]]$quantDat2
    if (dim(quantDat2)[1] < 100) {
        stopper(
            "Less than one hundred genes detected after",
            "mapping genes between species.",
            "Was ctd_species defined correctly?"
        )
    }
    return(quantDat2)
}
