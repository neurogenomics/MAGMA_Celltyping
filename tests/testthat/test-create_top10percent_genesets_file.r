test_that("create_top10percent_genesets_file works", {
    genesOutFile <- tempfile(fileext = ".genes.out")
    ctd <- ewceData::ctd()
    annotLevel <- 1
    data.table::fwrite(
        x = MAGMA.Celltyping::genesOut,
        file = genesOutFile,
        sep = "\t"
    )

    geneSetsFilePath <- MAGMA.Celltyping:::create_top10percent_genesets_file(
        genesOutFile = genesOutFile,
        ctd = ctd,
        annotLevel = annotLevel,
        ctd_species = "mouse"
    )
    geneSets <- readLines(geneSetsFilePath)
    #### One line per cell-type ####
    testthat::expect_equal(
        length(geneSets),
        ncol(ctd[[annotLevel]]$mean_exp)
    )
    #### That cell-type names are recoverable ####
    # NOTE! celltype names with spaces are problematic since spaces are used
    # as separators between genes in this file type.
    celltypes <- unlist(lapply(geneSets, function(x) {
        strsplit(x, " ")[[1]][1]
    }))
    testthat::expect_false(
        all(celltypes %in% colnames(ctd[[annotLevel]]$mean_exp))
    )
    testthat::expect_true(
        all(celltypes %in% gsub(" ", "_", colnames(ctd[[annotLevel]]$mean_exp)))
    )
})
