# Full workflow

``` r

library(MAGMA.Celltyping) 
```

    ## ===================== 🦠🌋🦠 Welcome to MAGMA.Celltyping 🦠🌋🦠 =====================
    ## This package uses MAGMA:
    ## https://ctg.cncr.nl/software/magma
    ## 
    ## To cite MAGMA.Celltyping, please use:
    ## * Skene, N.G., Bryois, J., Bakken, T.E. et al. Genetic identification of
    ##      brain cell types underlying schizophrenia. Nat Genet 50, 825-833 (2018).
    ##      https://doi.org/10.1038/s41588-018-0129-5
    ## * de Leeuw CA, Mooij JM, Heskes T, Posthuma D (2015) MAGMA: Generalized
    ##      Gene-Set Analysis of GWAS Data. PLOS Computational Biology 11(4): e1004219.
    ##      https://doi.org/10.1371/journal.pcbi.1004219
    ## 
    ## Please report any bugs or feature requests by filling out an Issues template:
    ##      https://github.com/neurogenomics/MAGMA_Celltyping/issues
    ## ===================== 🦠🌋🦠 =========================== 🦠🌋🦠 =====================

``` r

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

## Intro

`MAGMA.Celltyping` is a software package that facilitates conducting
cell-type-specific enrichment tests on GWAS summary statistics.

The following vignette shows the step-by-step version of the
`MAGMA.Celltyping` pipeline. For the newer streamlined version of the
pipeline (that wraps many of these functions), please see the *Getting
Started* vignette.

## Prepare data

### Import GWAS

- We need to have a GWAS summary statistics file to analyse as input.  
- As an example, you can download UK Biobank summary statistics for
  ‘fluid_intelligence’ using
  [`get_example_gwas()`](https://neurogenomics.github.io/MAGMA_Celltyping/reference/get_example_gwas.md).

``` r

gwas_sumstats_path <- MAGMA.Celltyping::get_example_gwas(
  trait = "fluid_intelligence",
  munged = FALSE)
```

### Munge GWAS

Our lab have created a robust Bioconductor package for formatting
multiple types of summary statistics files:
[`MungeSumstats`](https://github.com/neurogenomics/MungeSumstats)
(please cite the associated publication if you use this package):

> [Murphy, Schilder, & Skene, MungeSumstats: a Bioconductor package for
> the standardization and quality control of many GWAS summary
> statistics, Bioinformatics, Volume 37, Issue 23, 1 December 2021,
> Pages 4593–4596,
> https://doi.org/10.1093/bioinformatics/btab665](https://doi.org/10.1093/bioinformatics/btab665)

- We will use `MungeSumstats` here to reformat the summary statistics
  for Fluid Intelligence. The minimum info needed *after* munging is:  
- SNP, CHR, BP as first three columns.
- It has at least one of these columns:
  (“Z”,“OR”,“BETA”,“LOG_ODDS”,“SIGNED_SUMSTAT”)

Genome builds:

- The UK Biobank data from Ben Neale uses “GRCh37” so we will specify
  this.  
- However, if you don’t know which genome build you GWAS sum stats are
  in, `MungeSumstats` will infer this information by default (simply set
  `ref_genome=NULL`).

``` r

path_formatted <- MungeSumstats::format_sumstats(path=gwas_sumstats_path,
                                                 save_path = tempfile(fileext = ".formatted.tsv.gz"),
                                                 ref_genome ="GRCh37")
```

For the sake of this example, we also provide a pre-munged version of
the above file. If you use this, you can skip the previous two steps.

``` r

path_formatted_intelligence <- MAGMA.Celltyping::get_example_gwas(
    trait = "fluid_intelligence", 
    munged = TRUE)
```

### Map SNPs to Genes

Next, we must convert the GWAS summary statistics file to a gene-level
signature so that we can compare it to a gene-level transcriptomic
cell-type reference.

Note you can input the genome build of your summary statistics for this
step or it can be inferred if left `NULL`:

``` r

genesOutPath_intelligence <- MAGMA.Celltyping::map_snps_to_genes(
    path_formatted = gwas_sumstats_path,
    genome_build = "GRCh37")
```

### CellTypeDataset

#### Import CTD

The second step in `MAGMA.Celltyping` is to prepare your Cell Type
Dataset (CTD) reference, from which we will get gene signatures for each
cell-type in the reference.

The
[`ewceData`](https://bioconductor.org/packages/release/data/experiment/html/ewceData.html)
package comes with a CTD which we use as an example.

If you want to import your own single cell RNA-seq dataset, then this
needs converting into CTD format; please see the [`EWCE`
vingette](https://nathanskene.github.io/EWCE/articles/extended.html#create-a-celltypedataset)
for explanation of how to do this.

``` r

ctd <- ewceData::ctd()
```

Note that the cell type dataset loaded in the code above is the
Karolinksa cortex/hippocampus data only. For the full Karolinska dataset
with hypothalamus and midbrain instead use the following:

``` r

ctd <- MAGMA.Celltyping::get_ctd("ctd_allKI")
```

You can find a list of other datasets provided by this package using
[`?MAGMA.Celltyping::get_ctd`](https://neurogenomics.github.io/MAGMA_Celltyping/reference/get_ctd.md):

``` r

ctd <- get_ctd("ctd_Tasic")
ctd <- get_ctd("ctd_DivSeq")
ctd <- get_ctd("ctd_AIBS")
ctd <- get_ctd("ctd_DRONC_human")
ctd <- get_ctd("ctd_DRONC_mouse")
ctd <- get_ctd("ctd_BlueLake2018_FrontalCortexOnly")
ctd <- get_ctd("ctd_BlueLake2018_VisualCortexOnly")
ctd <- get_ctd("ctd_Saunders")
ctd <- get_ctd("ctd_Zeisel2018")
```

#### Prepare quantiles

Preparing quantiles in the CTD is now something that is automatically
handled by by `MAGMA.Celltyping` so the user doesn’t need to do this
themselves as a pre-step anymore.

However, we still export the function `prepare_quantile_groups` in case
users want to have more control.

`prepare_quantile_groups` calculates the quantile groups for each
celltype within the single-cell dataset. If your single-cell dataset is
not from mouse, then change the `ctd_species` argument.

``` r

ctd <- MAGMA.Celltyping::prepare_quantile_groups(ctd = ctd,
                                                 ctd_species="mouse")
# Examine how the quantile groups look
print(ctd[[1]]$specificity_quantiles[c("Gfap","Dlg4","Aif1"),])
print(table(ctd[[1]]$specificity_quantiles[,1]))
```

## Run the main cell type association analysis

The analyses can be run in either linear or top10% enrichment modes.
Each have their own advantages/disadvantages.

- **Linear mode**: Performs a linear regression analysis using
  continuous gene-association scores from the GWAS MAGMA results and
  continuous specificity scores from each cell-type in the CTD
  reference. This mode takes advantage of gene-level weights in order to
  get more accurate association estimates. However, this mode also tends
  to give more significant results in general, as it may be capturing
  signatures from multiple cell-types at once.  
- **Top 10% mode**: Performs gene-set enrichment analysis (GSEA) using
  only the top 10% most cell-type-specific genes for each cell-type
  (i.e. genes in the top specificity decile). Unlike linear mode, top
  10% mode discards any gene-level weight data during testing and
  instead treats genes as categorical data (in the gene list or not in
  the gene list). While you do lose the nuance of the gene-level
  weights, you potential capture less cell-type signatures in a given
  test (due to only using a handful of top genes), which can make
  interpretation easier.

Let’s start with linear:

``` r

ctAssocsLinear <- MAGMA.Celltyping::calculate_celltype_associations(
  ctd = ctd,
  gwas_sumstats_path = path_formatted_intelligence, 
  ctd_species = "mouse")

FigsLinear <- MAGMA.Celltyping::plot_celltype_associations(
  ctAssocs = ctAssocsLinear,
  ctd = ctd)
```

Now let’s add the top 10% mode

``` r

ctAssocsTop <- MAGMA.Celltyping::calculate_celltype_associations(
  ctd = ctd,
  gwas_sumstats_path = path_formatted_intelligence, 
  EnrichmentMode = "Top 10%")

FigsTopDecile <- MAGMA.Celltyping::plot_celltype_associations(
  ctAssocs = ctAssocsTop,
  ctd = ctd)
```

Then plot linear together with the top 10% mode.

``` r

ctAssocMerged <- MAGMA.Celltyping::merge_magma_results(
  ctAssoc1 = ctAssocsLinear,
  ctAssoc2 = ctAssocsTop)

FigsMerged <- MAGMA.Celltyping::plot_celltype_associations(
  ctAssocs = ctAssocMerged,
  ctd = ctd)
```

#### Conditional analysis (linear mode)

Sometimes a single cell-type can dominate the signature in a given GWAS
(e.g. pyramidal cells in Schizophrenia). This makes it difficult to
disentangle whether other cell-types are truly associated with the GWAS
trait, or whether the are simply enriched because they have an
overlapping signature with the top cell-type. Conditional mode allows
you to control for one (or several) cell-type(s) and see whether other
cell-types still remain significantly enriched.

By default, it is assumed that you want to run the linear enrichment
analysis. There are two modes for conditional analyses, you can either
control for the top N cell types from the baseline analysis (in which
case, set `controlTopNcells`) or control for specific specified cell
types (in which case, set `controlledCTs`).

``` r

ctCondAssocs <- MAGMA.Celltyping::calculate_conditional_celltype_associations(
  ctd = ctd,
  gwas_sumstats_path = path_formatted_intelligence, 
  analysis_name = "Conditional",
  controlTopNcells= 2)

if(!is.null(ctCondAssocs)){ 
    MAGMA.Celltyping::plot_celltype_associations(ctAssocs = ctCondAssocs,
                                                 ctd = ctd)
}
```

Let’s try as an alternative to control for expression of 3 neuronal
subtypes at the same time:

``` r

ctCondAssocs <- MAGMA.Celltyping::calculate_conditional_celltype_associations(
  ctd = ctd,
  gwas_sumstats_path = path_formatted_intelligence, 
  analysis_name = "Conditional",
  controlledCTs = c("pyramidal CA1","pyramidal SS","interneurons"),
  controlledAnnotLevel = 1)

MAGMA.Celltyping::plot_celltype_associations(ctAssocs = ctCondAssocs,
                                             ctd=ctd)
```

Note that Periventricular Microglia (PVM) go from totally
non-significant to significant once the neurons are controlled for. Test
if this change is significant as follows:

``` r

magma1 = ctCondAssocs[[2]]$results[ctCondAssocs[[2]]$results$CONTROL=="BASELINE",]
magma2 = ctCondAssocs[[2]]$results[ctCondAssocs[[2]]$results$CONTROL=="interneurons,pyramidal_CA1,pyramidal_SS",]

resCompared <- MAGMA.Celltyping:::compare_trait_enrichments(magma1=magma1,
                                                            magma2=magma2,
                                                            annotLevel=2,
                                                            ctd=ctd)
resCompared[1:3,]
```

Using this approach we can see that the increased enrichment is
microglia in the controlled analysis is almost significantly increased
relative to the baseline analysis.

### Conditional analyses (top 10% mode)

Conditional analyses can also be performed with top 10% mode (although
the conditioning is done in linear mode).

``` r

ctCondAssocsTopTen = MAGMA.Celltyping::calculate_conditional_celltype_associations(
  ctd = ctd,
  gwas_sumstats_path = path_formatted_intelligence, 
  analysis_name = "Conditional",
  controlledCTs=c("pyramidal CA1","pyramidal SS","interneurons"),
  controlledAnnotLevel=1,
  EnrichmentMode = "Top 10%")

MAGMA.Celltyping::plot_celltype_associations(ctAssocs = ctCondAssocsTopTen,
                                             ctd = ctd)
```

### Controlling for a second GWAS

We now want to test enrichments that remain in a GWAS after we control
for a second GWAS. So let’s download a second GWAS sumstats file and
prepare it for analysis.

20016.assoc.tsv is the sumstats file for ‘Fluid Intelligence Score’ from
the UK Biobank.

So let’s subtract genes associated with prospective memory from those
involved in fluid intelligence.

#### Download and prepare the ‘Prospective memory’ GWAS summary statistics

``` r

#### Download pre-munged GWAS summary stats ####
gwas_sumstats_path_memory <- MAGMA.Celltyping::get_example_gwas(
    trait = "prospective_memory",
    munged = FALSE)
#### Format summary stats ####
## and infer the genome build from the data whilst doing so (see output messages)
path_formatted_memory <- MungeSumstats::format_sumstats(
    path = gwas_sumstats_path_memory)

#### Can also skip ahead by importing pre-munged summary stats ####
# path_formatted_memory <- MAGMA.Celltyping::get_example_gwas(
#     trait = "prospective_memory",
#     munged = TRUE)

#### Map SNPs to genes ####
genesOutPath_memory <- MAGMA.Celltyping::map_snps_to_genes(
    path_formatted = path_formatted_memory,
    genome_build = "GRCH37")
```

#### Check which cell types this GWAS is associated with at baseline

``` r

ctAssocsLinear_memory <- MAGMA.Celltyping::calculate_celltype_associations(
  ctd = ctd,
  gwas_sumstats_path = path_formatted_memory, 
  ctd_species = "mouse")

ctAssocsLinear_intelligence <- MAGMA.Celltyping::calculate_celltype_associations(
  ctd = ctd,
  gwas_sumstats_path = path_formatted_intelligence, 
  ctd_species = "mouse")

MAGMA.Celltyping::plot_celltype_associations(ctAssocs = ctAssocsLinear_memory,
                                             ctd = ctd)
```

#### Compare enrichments in the two GWAS using a tile plot

``` r

ctAssocMerged_MemInt <- MAGMA.Celltyping::merge_magma_results(
  ctAssoc1 = ctAssocsLinear_memory,
  ctAssoc2 = ctAssocsLinear_intelligence)

FigsMerged_MemInt <- lapply(seq_len(length(ctd)), 
                            function(lvl){
    MAGMA.Celltyping::magma_tileplot(ctd=ctd,
                          results=ctAssocMerged_MemInt[[lvl]]$results,
                          annotLevel=lvl,
                          fileTag=paste0("Merged_MemInt_lvl",lvl))
}) 
```

#### Check which cell types ‘Fluid Intelligence’ is associated with after controlling for ‘Prospective memory’

``` r

# Set paths for GWAS sum stats + .genes.out file (with the z-scores)  
ctAssocsLinear <- MAGMA.Celltyping::calculate_celltype_associations(
  ctd = ctd,
  ctd_species = "mouse", 
  gwas_sumstats_path = path_formatted_intelligence, 
  genesOutCOND = genesOutPath_memory,
  analysis_name = "ControllingForPropMemory")

FigsLinear <- MAGMA.Celltyping::plot_celltype_associations(
    ctAssocs = ctAssocsLinear,
    ctd = ctd,
    fileTag = "ControllingForPropMemory")
```

We find that after controlling for prospective memory, there is no
significant enrichment left associated with fluid intelligence.

### Calculate cell type enrichments directly (using linear model)

Instead of relying on the tool MAGMA (which requires an extra
installation step), we can also compute cell-type enrichment scores in
an R-native re-implementation of the MAGMA enrichment analyses using the
following functions:

``` r

ctd <- ewceData::ctd()
magma_GenesOut_file <- MAGMA.Celltyping::import_magma_files(
    file_types = "genes.out",
    return_dir = FALSE)

magmaAdjZ <- MAGMA.Celltyping:: adjust_zstat_in_genesOut(
  ctd = ctd,
  ctd_species = "mouse",
  magma_GenesOut_file = magma_GenesOut_file)

output <- MAGMA.Celltyping:: calculate_celltype_enrichment_limma(
  magmaAdjZ = magmaAdjZ,
  ctd = ctd,
  thresh = 0.0001,
  ctd_species = "mouse",
  annotLevel = 2)
```

We can then get the probability of the celltype being enriched as
follows

``` r

print(sort(output))
```

The results should closely resemble those obtained using MAGMA

### Gene set enrichments

To test whether a gene set is enriched using MAGMA the following
commands can be used:

``` r

data("rbfox_binding")
 
geneset_res <- MAGMA.Celltyping::calculate_geneset_enrichment(
  geneset = rbfox_binding,
  gwas_sumstats_path = path_formatted_intelligence,
  analysis_name = "Rbfox_20016",
  geneset_species = "mouse")
print(geneset_res)
```

We can then test whether the gene set is still enriched after
controlling for celltype enrichment:

``` r

ctd <- MAGMA.Celltyping::get_ctd("ctd_allKI")

analysis_name <- "Rbfox_16_pyrSS"
controlledCTs <- c("pyramidal SS")

cond_geneset_res_pyrSS = MAGMA.Celltyping::calculate_conditional_geneset_enrichment(
  geneset = geneset,
  ctd = ctd,
  controlledAnnotLevel = 1,
  controlledCTs = controlledCTs,
  gwas_sumstats_path = gwas_sumstats_path,
  analysis_name = analysis_name, 
  ctd_species = "mouse")

controlledCTs <- c("pyramidal CA1")
cond_geneset_res_pyrCA1 <- MAGMA.Celltyping::calculate_conditional_geneset_enrichment(
  geneset = geneset,
  ctd = ctd,
  controlledAnnotLevel = 1,
  controlledCTs = controlledCTs,
  gwas_sumstats_path = gwas_sumstats_path,
  analysis_name = analysis_name, 
  ctd_species = "mouse")

controlledCTs <- c("pyramidal CA1","pyramidal SS")
cond_geneset_res_pyr = MAGMA.Celltyping::calculate_conditional_geneset_enrichment(
  geneset = geneset,
  ctd = ctd,
  controlledAnnotLevel = 1,
  controlledCTs = controlledCTs,
  gwas_sumstats_path = gwas_sumstats_path,
  analysis_name = analysis_name, 
  ctd_species = "mouse")

controlledCTs <- c("Medium Spiny Neuron")
cond_geneset_res_MSN = MAGMA.Celltyping::calculate_conditional_geneset_enrichment(
  geneset = geneset,
  ctd = ctd,
  controlledAnnotLevel = 1,
  controlledCTs = controlledCTs,
  gwas_sumstats_path = gwas_sumstats_path,
  analysis_name = analysis_name, 
  ctd_species = "mouse")

controlledCTs <- c("Medium Spiny Neuron","pyramidal CA1","pyramidal SS","interneurons")
cond_geneset_res = MAGMA.Celltyping::calculate_conditional_geneset_enrichment(
  geneset = geneset,
  ctd = ctd,
  controlledAnnotLevel = 1,
  controlledCTs = controlledCTs,
  gwas_sumstats_path = gwas_sumstats_path,
  analysis_name = analysis_name, 
  ctd_species = "mouse")
```

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
    ## [1] dplyr_1.1.4             MAGMA.Celltyping_2.0.16 BiocStyle_2.39.0       
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
    ##  [67] BiocManager_1.30.27         fastmap_1.2.0              
    ##  [69] boot_1.3-32                 digest_0.6.39              
    ##  [71] R6_2.6.1                    gridGraphics_0.5-1         
    ##  [73] textshaping_1.0.4           RSQLite_2.4.5              
    ##  [75] cigarillo_1.1.0             R.methodsS3_1.8.2          
    ##  [77] tidyr_1.3.2                 generics_0.1.4             
    ##  [79] fontLiberation_0.1.0        data.table_1.18.0          
    ##  [81] rtracklayer_1.71.3          httr_1.4.7                 
    ##  [83] htmlwidgets_1.6.4           S4Arrays_1.11.1            
    ##  [85] pkgconfig_2.0.3             gtable_0.3.6               
    ##  [87] blob_1.3.0                  S7_0.2.1                   
    ##  [89] SingleCellExperiment_1.33.0 XVector_0.51.0             
    ##  [91] htmltools_0.5.9             fontBitstreamVera_0.1.1    
    ##  [93] carData_3.0-5               bookdown_0.46              
    ##  [95] scales_1.4.0                Biobase_2.71.0             
    ##  [97] png_0.1-8                   reformulas_0.4.3.1         
    ##  [99] ggfun_0.2.0                 ggdendro_0.2.0             
    ## [101] knitr_1.51                  reshape2_1.4.5             
    ## [103] rjson_0.2.23                nloptr_2.2.1               
    ## [105] nlme_3.1-168                curl_7.0.0                 
    ## [107] cachem_1.1.0                stringr_1.6.0              
    ## [109] BiocVersion_3.23.1          parallel_4.6.0             
    ## [111] AnnotationDbi_1.73.0        restfulr_0.0.16            
    ## [113] desc_1.4.3                  pillar_1.11.1              
    ## [115] grid_4.6.0                  vctrs_0.7.1                
    ## [117] ggpubr_0.6.2                car_3.1-3                  
    ## [119] dbplyr_2.5.1                evaluate_1.0.5             
    ## [121] orthogene_1.17.2            GenomicFeatures_1.63.1     
    ## [123] cli_3.6.5                   compiler_4.6.0             
    ## [125] Rsamtools_2.27.0            rlang_1.1.7                
    ## [127] crayon_1.5.3                ggsignif_0.6.4             
    ## [129] gprofiler2_0.2.4            EWCE_1.19.0                
    ## [131] ieugwasr_1.1.0              plyr_1.8.9                 
    ## [133] fs_1.6.6                    ggiraph_0.9.3              
    ## [135] stringi_1.8.7               viridisLite_0.4.2          
    ## [137] ewceData_1.19.0             BiocParallel_1.45.0        
    ## [139] babelgene_22.9              Biostrings_2.79.4          
    ## [141] lazyeval_0.2.2              gh_1.5.0                   
    ## [143] homologene_1.4.68.19.3.27   fontquiver_0.2.1           
    ## [145] Matrix_1.7-4                ExperimentHub_3.1.0        
    ## [147] MungeSumstats_1.19.5        BSgenome_1.79.1            
    ## [149] patchwork_1.3.2             bit64_4.6.0-1              
    ## [151] ggplot2_4.0.1               KEGGREST_1.51.1            
    ## [153] statmod_1.5.1               SummarizedExperiment_1.41.0
    ## [155] AnnotationHub_4.1.0         rbibutils_2.4.1            
    ## [157] broom_1.0.11                memoise_2.0.1              
    ## [159] bslib_0.10.0                ggtree_4.1.1               
    ## [161] bit_4.6.0                   splitstackshape_1.4.8      
    ## [163] ape_5.8-1
