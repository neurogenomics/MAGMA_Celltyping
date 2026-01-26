# Docker/Singularity Containers

## Installation

MAGMA.Celltyping is now available via
[ghcr.io](https://ghcr.io/ghcr.io/neurogenomics/MAGMA.Celltyping) as a
containerised environment with Rstudio and all necessary dependencies
pre-installed.

### Method 1: via Docker

First, [install Docker](https://docs.docker.com/get-docker/) if you have
not already.

Create an image of the [Docker](https://www.docker.com/) container in
command line:

    docker pull ghcr.io/neurogenomics/MAGMA.Celltyping

Once the image has been created, you can launch it with:

    docker run \
      -d \
      -e ROOT=true \
      -e PASSWORD="<your_password>" \
      -v ~/Desktop:/Desktop \
      -v /Volumes:/Volumes \
      -p 8900:8787 \
      ghcr.io/neurogenomics/MAGMA.Celltyping

#### NOTES

- Make sure to replace `<your_password>` above with whatever you want
  your password to be.  
- Change the paths supplied to the `-v` flags for your particular use
  case.
- The `-d` ensures the container will run in “detached” mode, which
  means it will persist even after you’ve closed your command line
  session.  
- The username will be *“rstudio”* by default.  
- Optionally, you can also install the [Docker
  Desktop](https://www.docker.com/products/docker-desktop/) to easily
  manage your containers.

### Method 2: via Singularity

If you are using a system that does not allow Docker (as is the case for
many institutional computing clusters), you can instead [install Docker
images via
Singularity](https://docs.sylabs.io/guides/2.6/user-guide/singularity_and_docker.html).

    singularity pull docker://ghcr.io/neurogenomics/MAGMA.Celltyping

For troubleshooting, see the [Singularity
documentation](https://docs.sylabs.io/guides/latest/user-guide/singularity_and_docker.html#github-container-registry).

## Usage

Finally, launch the containerised Rstudio by entering the following URL
in any web browser: *<http://localhost:8900/>*

Login using the credentials set during the Installation steps.

## Session Info

``` r

utils::sessionInfo()
```

    ## R Under development (unstable) (2026-01-25 r89330)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.3 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] MAGMA.Celltyping_2.0.16 BiocStyle_2.39.0       
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] splines_4.6.0               BiocIO_1.21.0              
    ##   [3] bitops_1.0-9                ggplotify_0.1.3            
    ##   [5] filelock_1.0.3              tibble_3.3.1               
    ##   [7] R.oo_1.27.1                 XML_3.99-0.20              
    ##   [9] lifecycle_1.0.5             httr2_1.2.2                
    ##  [11] Rdpack_2.6.5                rstatix_0.7.3              
    ##  [13] lattice_0.22-7              MASS_7.3-65                
    ##  [15] backports_1.5.0             magrittr_2.0.4             
    ##  [17] limma_3.67.0                plotly_4.12.0              
    ##  [19] sass_0.4.10                 rmarkdown_2.30             
    ##  [21] jquerylib_0.1.4             yaml_2.3.12                
    ##  [23] otel_0.2.0                  HGNChelper_0.8.15          
    ##  [25] minqa_1.2.8                 DBI_1.2.3                  
    ##  [27] RColorBrewer_1.1-3          abind_1.4-8                
    ##  [29] GenomicRanges_1.63.1        purrr_1.2.1                
    ##  [31] R.utils_2.13.0              BiocGenerics_0.57.0        
    ##  [33] RCurl_1.98-1.17             yulab.utils_0.2.3          
    ##  [35] VariantAnnotation_1.57.1    rappdirs_0.3.4             
    ##  [37] gdtools_0.4.4               IRanges_2.45.0             
    ##  [39] S4Vectors_0.49.0            tidytree_0.4.7             
    ##  [41] pkgdown_2.2.0               codetools_0.2-20           
    ##  [43] DelayedArray_0.37.0         tidyselect_1.2.1           
    ##  [45] aplot_0.2.9                 UCSC.utils_1.7.1           
    ##  [47] farver_2.1.2                lme4_1.1-38                
    ##  [49] matrixStats_1.5.0           stats4_4.6.0               
    ##  [51] BiocFileCache_3.1.0         Seqinfo_1.1.0              
    ##  [53] GenomicAlignments_1.47.0    jsonlite_2.0.0             
    ##  [55] Formula_1.2-5               systemfonts_1.3.1          
    ##  [57] tools_4.6.0                 treeio_1.35.0              
    ##  [59] ragg_1.5.0                  Rcpp_1.1.1                 
    ##  [61] glue_1.8.0                  SparseArray_1.11.10        
    ##  [63] xfun_0.56                   MatrixGenerics_1.23.0      
    ##  [65] GenomeInfoDb_1.47.2         RNOmni_1.0.1.2             
    ##  [67] dplyr_1.1.4                 BiocManager_1.30.27        
    ##  [69] fastmap_1.2.0               boot_1.3-32                
    ##  [71] digest_0.6.39               R6_2.6.1                   
    ##  [73] gridGraphics_0.5-1          textshaping_1.0.4          
    ##  [75] RSQLite_2.4.5               cigarillo_1.1.0            
    ##  [77] R.methodsS3_1.8.2           tidyr_1.3.2                
    ##  [79] generics_0.1.4              fontLiberation_0.1.0       
    ##  [81] data.table_1.18.0           rtracklayer_1.71.3         
    ##  [83] httr_1.4.7                  htmlwidgets_1.6.4          
    ##  [85] S4Arrays_1.11.1             pkgconfig_2.0.3            
    ##  [87] gtable_0.3.6                blob_1.3.0                 
    ##  [89] S7_0.2.1                    SingleCellExperiment_1.33.0
    ##  [91] XVector_0.51.0              htmltools_0.5.9            
    ##  [93] fontBitstreamVera_0.1.1     carData_3.0-5              
    ##  [95] bookdown_0.46               scales_1.4.0               
    ##  [97] Biobase_2.71.0              png_0.1-8                  
    ##  [99] reformulas_0.4.3.1          ggfun_0.2.0                
    ## [101] ggdendro_0.2.0              knitr_1.51                 
    ## [103] reshape2_1.4.5              rjson_0.2.23               
    ## [105] nloptr_2.2.1                nlme_3.1-168               
    ## [107] curl_7.0.0                  cachem_1.1.0               
    ## [109] stringr_1.6.0               BiocVersion_3.23.1         
    ## [111] parallel_4.6.0              AnnotationDbi_1.73.0       
    ## [113] restfulr_0.0.16             desc_1.4.3                 
    ## [115] pillar_1.11.1               grid_4.6.0                 
    ## [117] vctrs_0.7.1                 ggpubr_0.6.2               
    ## [119] car_3.1-3                   dbplyr_2.5.1               
    ## [121] evaluate_1.0.5              orthogene_1.17.2           
    ## [123] GenomicFeatures_1.63.1      cli_3.6.5                  
    ## [125] compiler_4.6.0              Rsamtools_2.27.0           
    ## [127] rlang_1.1.7                 crayon_1.5.3               
    ## [129] ggsignif_0.6.4              gprofiler2_0.2.4           
    ## [131] EWCE_1.19.0                 ieugwasr_1.1.0             
    ## [133] plyr_1.8.9                  fs_1.6.6                   
    ## [135] ggiraph_0.9.3               stringi_1.8.7              
    ## [137] viridisLite_0.4.2           ewceData_1.19.0            
    ## [139] BiocParallel_1.45.0         babelgene_22.9             
    ## [141] Biostrings_2.79.4           lazyeval_0.2.2             
    ## [143] gh_1.5.0                    homologene_1.4.68.19.3.27  
    ## [145] fontquiver_0.2.1            Matrix_1.7-4               
    ## [147] ExperimentHub_3.1.0         MungeSumstats_1.19.5       
    ## [149] BSgenome_1.79.1             patchwork_1.3.2            
    ## [151] bit64_4.6.0-1               ggplot2_4.0.1              
    ## [153] KEGGREST_1.51.1             statmod_1.5.1              
    ## [155] SummarizedExperiment_1.41.0 AnnotationHub_4.1.0        
    ## [157] rbibutils_2.4.1             broom_1.0.11               
    ## [159] memoise_2.0.1               bslib_0.10.0               
    ## [161] ggtree_4.1.1                bit_4.6.0                  
    ## [163] splitstackshape_1.4.8       ape_5.8-1

  
