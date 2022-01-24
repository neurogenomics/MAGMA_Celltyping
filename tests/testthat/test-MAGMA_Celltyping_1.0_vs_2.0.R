test_that("old (1.0.0) vs new (>=2.0) MAGMA.Celltyping versions produce the same results", {
    
    ### Used an older version of MAGMA executable (v1.07b) when gathering
    ## MAGMA.Celltyping 1.0.0 results, so that it could be run with minimal errors. 
    
    ## Unnecessary to run every time, more of a one-time comparison.
    run_comparison <- FALSE
    if(!is_32bit() & run_comparison){
        #### Script used to produce the results from MAGMA_Celltyping 1.0.0 ####
         # system.file("extdata/MAGMA_Celltyping_1.0_vignette.R", package="MAGMA.Celltyping")
        set.seed(2020)
        ctd <- ewceData::ctd()
        upstream_kb = 10 
        downstream_kb = 1.5
        zipfile <- "MAGMA_Celltyping_1.0_results.zip"
        piggyback::pb_download(file = zipfile, 
                               dest = tempdir(),
                               repo = "neurogenomics/MAGMA_Celltyping")
        unzip(zipfile = file.path(tempdir(),zipfile), 
              exdir = tempdir())
        
        storage_dir <- gsub(".zip","",file.path(tempdir(),zipfile))
        magma_dir <- file.path(storage_dir,"GWAS/MAGMA_Files",
                               "20016_irnt.gwas.imputed_v3.both_sexes.formatted.tsv.10UP.1.5DOWN")
        cor_res <- function(res1,
                            res2, 
                            thresh=.90,
                            fillNA=0){
            common_cts <- intersect(rownames(res1), rownames(res2))
            all_cts <- union(rownames(res1), rownames(res2))
            if(length(common_cts)>1){
                message(length(common_cts)," / ",length(all_cts)," overlapping cell-types.")
            } else {
                message("<2 overlapping cell-types.")
                return(NULL)
            }
            if(!is.na(fillNA)){
                res1$P[is.na(res1$P)] <- fillNA
                res2$P[is.na(res2$P)] <- fillNA
            }
            r <- cor(res1[common_cts,]$P,
                     res2[common_cts,]$P,
                     use = "complete.obs")
            message("corr = ",round(r,2))
            testthat::expect_gte(r,thresh)
        }
            
        #### Run the main cell type association analysis ####
        ctAssocsLinear1 = calculate_celltype_associations(ctd=ctd,
                                                         magma_dir = magma_dir,
                                                         upstream_kb = upstream_kb, 
                                                         downstream_kb = downstream_kb,
                                                         EnrichmentMode = "Linear",
                                                         ctd_species = "mouse")
        ctAssocsLinear2 <- readRDS(file.path(storage_dir,"ctAssocsLinear.rds"))
        for(i in seq_len(length(ctd))){
            message("Testing CTD level: ",i) 
            res1 <- subset(ctAssocsLinear1[[i]]$results, P<.05)
            res2 <- subset(ctAssocsLinear2[[i]]$results, P<.05)
            ct1 <- rownames(dplyr::slice_max(res1, order_by = dplyr::all_of(P), n = 3))
            ct2 <- rownames(dplyr::slice_max(res2, order_by =  dplyr::all_of(P), n = 3))
            testthat::expect_equal(ct1, ct2)
            ## Test that results are at least correlated ##
            cor_res(res1 = res1, 
                    res2 = res2)
        }
        
        
        #### Now let's add the top 10% mode ####
        # We expect top 10% mode to be different because in the old version, 
        # specificity deciles were being computed incorrectly.  
        ctAssocsTop1 = calculate_celltype_associations(ctd=ctd,
                                                      magma_dir = magma_dir, 
                                                      upstream_kb = upstream_kb, 
                                                      downstream_kb = downstream_kb,
                                                      EnrichmentMode="Top 10%")
        ctAssocsTop2 <- readRDS(file.path(storage_dir,"ctAssocsTop.rds"))
        for(i in seq_len(length(ctd))){
            message("Testing CTD level: ",i)
            res1 <- subset(ctAssocsTop1[[i]]$results, P<.05)
            res2 <- subset(ctAssocsTop2[[i]]$results, P<.05)
            ct1 <- res1$Celltype
            ct2 <- res2$Celltype
            testthat::expect_true(sum(!ct1 %in% ct2)>1)
            testthat::expect_true(sum(ct1 %in% ct2)>0) 
            ## Test that results are at least correlated ##
            cor_res(res1 = res1, 
                    res2 = res2,
                    thresh = .70)
        }
        
        
        ## The merged results will therefore will have 
        ## a mix of matching and non-matchig results
        ctAssocMerged1 = merge_magma_results(ctAssoc1=ctAssocsLinear1,
                                             ctAssoc2=ctAssocsTop1)
        ctAssocMerged2 <- readRDS(file.path(storage_dir,"ctAssocMerged.rds"))
        for(i in seq_len(length(ctd))){
            message("Testing CTD level: ",i)
            res1 <- subset(ctAssocMerged1[[i]]$results, P<.05)
            res2 <- subset(ctAssocMerged2[[i]]$results, P<.05)
            ct1 <- rownames(dplyr::slice_max(res1, order_by = dplyr::all_of(P), n = 3))
            ct2 <- rownames(dplyr::slice_max(res2, order_by =  dplyr::all_of(P), n = 3))
            testthat::expect_true(sum(!ct1 %in% ct2)>0)
            testthat::expect_true(sum(ct1 %in% ct2)>0)
            ## Test that results are at least correlated ##
            cor_res(res1 = res1, 
                    res2 = res2,
                    thresh = .70)
        }
        
        
        
        
        ### Run the conditional cell type association analysis (linear mode)
        
        # By default, it is assumed that you want to run the linear enrichment analysis. There are two modes for conditional analyses, you can either control for the top N cell types from the baseline analysis (in which case, set controlTopNcells) or control for specific specified cell types (in which case, set controlledCTs).
        
        # Conditional analysis
        ctCondAssocs1 = calculate_conditional_celltype_associations(ctd = ctd,
                                                                    magma_dir = magma_dir,
                                                                    upstream_kb = upstream_kb, 
                                                                    downstream_kb = downstream_kb,
                                                                    analysis_name = "Conditional",
                                                                    controlTopNcells = 2)
        ctCondAssocs2 <- readRDS(file.path(storage_dir,"ctCondAssocs_controlTopNcells-2.rds"))
        for(i in seq_len(length(ctd))){
            message("Testing CTD level: ",i)
            res1 <- subset(ctCondAssocs1[[i]]$results, P<.05)
            res2 <- subset(ctCondAssocs2[[i]]$results, P<.05)
            ct1 <- rownames(dplyr::slice_max(res1, order_by = dplyr::all_of(P), n = 3))
            ct2 <- rownames(dplyr::slice_max(res2, order_by =  dplyr::all_of(P), n = 3))
            testthat::expect_true(sum(!ct1 %in% ct2)>0)
            testthat::expect_true(sum(ct1 %in% ct2)>0)
            ## Test that results are at least correlated ##
            cor_res(res1 = res1, 
                    res2 = res2,
                    thresh = .70)
        }
        
        # Let's try as an alternative to control for expression of both the level 1 pyramidal neuron types at the same time
        controlledCTs <- sort(c("pyramidal CA1","pyramidal SS","interneurons"))
        ctCondAssocs1 = calculate_conditional_celltype_associations(ctd = ctd, 
                                                                   magma_dir = magma_dir,
                                                                   upstream_kb = upstream_kb, 
                                                                   downstream_kb = downstream_kb,
                                                                   analysis_name = "Conditional",
                                                                   controlledCTs=controlledCTs,
                                                                   controlledAnnotLevel=1)
        ct_filename <- paste(gsub(" ",".",controlledCTs),collapse = ",")
        ctCondAssocs2 <- readRDS(file.path(storage_dir,paste0("ctCondAssocs_controlledCTs-",ct_filename,".rds")))
        for(i in seq_len(length(ctd))){
            message("Testing CTD level: ",i)
            res1 <- subset(ctCondAssocs1[[i]]$results, P<.05)
            res2 <- subset(ctCondAssocs2[[i]]$results, P<.05)
            ct1 <- rownames(dplyr::slice_max(res1, order_by = dplyr::all_of(P), n = 3))
            ct2 <- rownames(dplyr::slice_max(res2, order_by =  dplyr::all_of(P), n = 3))
            testthat::expect_true(sum(!ct1 %in% ct2)>0)
            testthat::expect_true(sum(ct1 %in% ct2)>0)
            # Test celltypes, not subtypes
            testthat::expect_true(sum(stringr::str_sub(ct1,end = 3) %in% stringr::str_sub(ct2,end = 3))>1)
            ## Test that results are at least correlated ##
            cor_res(res1 = res1, 
                    res2 = res2,
                    thresh = .90)
        }
        
        
         
        
        #### Conditional analyses (top 10% mode) ####
        
        # Conditional analyses can also be performed with top 10% mode (although the conditioning is done in linear mode)
        ctCondAssocsTopTen1 = calculate_conditional_celltype_associations(ctd = ctd,
                                                                         magma_dir = magma_dir,
                                                                         analysis_name = "Conditional",
                                                                         controlledCTs=controlledCTs,
                                                                         upstream_kb = upstream_kb, 
                                                                         downstream_kb = downstream_kb,
                                                                         controlledAnnotLevel=1,
                                                                         EnrichmentMode = "Top 10%")
        ctCondAssocsTopTen2 <- readRDS(file.path(storage_dir,"rctCondAssocsTopTen.rds"))
        for(i in seq_len(length(ctd))){
            message("Testing CTD level: ",i)
            res1 <- subset(ctCondAssocsTopTen1[[i]]$results, P<.05)
            res2 <- subset(ctCondAssocsTopTen2[[i]]$results, P<.05)
            ct1 <- rownames(dplyr::slice_max(res1, order_by = dplyr::all_of(P), n = 3))
            ct2 <- rownames(dplyr::slice_max(res2, order_by =  dplyr::all_of(P), n = 3))
            testthat::expect_true(sum(!ct1 %in% ct2)>0) 
            # testthat::expect_true(sum(ct1 %in% ct2)>0) 
            # Test celltypes, not subtypes
            testthat::expect_true(sum(stringr::str_sub(ct1,end = 3) %in% stringr::str_sub(ct2,end = 3))>0)
            ## Test that results are at least correlated ##
            cor_res(res1 = res1, 
                    res2 = res2,
                    thresh = .70)
        } 
        
    } else {
        testthat::expect_null(NULL)
    }
})
