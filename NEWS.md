# MAGMA.Celltyping 2.0.1

* MAJOR UPGRADE: `MAGMA.Celltyping` was revamped to meet CRAN standards,
automatically install MAGMA, and take any species are input.

## New features

* Added a `NEWS.md` file to track changes to the package.
* Automatically install MAGMA with new `magma_install` function; 
stores binaries in `MAGMA.Celltyping`-specific cache dir. 
Added various support functions to make this possible and ensure 
correct version is being used.
* Allow `MAGMA.Celltyping` to install even if MAGMA is not installed. Instead,
check at the beginning of functions that require MAGMA using `magma_check`.
* Call MAGMA commands with `magma_run` which finds the requested version of 
MAGMA and uses it. 
* Print readable, user-friendly MAGMA commands when being run through
`magma_cmd` function. 
* Added unit tests.
* Create hex sticker
* New function: `get_sub_SNP_LOC_DATA`
* Formally deprecated functions using `.Deprecated` function:
    - `get_genomebuild_for_sumstats`
    - `build_snp_location_tables`
    - `format.sumstats.for.magma`
    - `format_sumstats_for_magma_macOnly`
    - `standardise.sumstats.column.headers`
    - `standardise.sumstats.column.headers.crossplatform`
* Renamed all functions with "." to "_" to meet coding standards.
* Renamed functions to be more concise and avoid issues with 
test file names being too long:
    - `calculate.celltype.enrichment.probabilities.wtLimma` --> `calculate_celltype_enrichment_limma`
    - `calculate.conditional.celltype.enrichment.probabilities.wtLimma` --> `calculate_conditional_celltype_enrichment_limma`
* Removed all large *data/* to GitHub Releases, now accessible with dedicated 
`piggyback`-based functions:
    - `get_ctd`: CellTypeDatasets
    - `get_example_gwas`: GWAS summary stats
    - `get_genomeLocFile`: NCBI gene coordinate references.
* Create example full GWAS summary stats (both unfiltered and filtered + munged with `MungeSumstats`). Accessed by `get_example_gwas`.
    - "prospective_memory"
    - "fluid_intelligence"
    - "educational_attainment"
* Updated vignettes:
    - Created concise *Getting started* vignette.
    - Updated origiinal vignette and turned into *full_workflow* vignette.
* Made certain functions run automatically internally, 
instead of having the user run them:  
    - `get_genome_ref`
    - `prepare_quantile_groups`
* Remove unncessary dependencies:
    - `reshape`
    - `cowplot`
    - `SNPlocs.Hsapiens.dbSNP144.GRCh37`
    - `SNPlocs.Hsapiens.dbSNP144.GRCh38`
* Replaced `hgnc2entrez` with improved `hgnc2entrez_ortohgene` from 
`orthogene::all_genes`. Benchmarked to confirm that the latter 
increases the number of genes that can be converted. 
* Allow all functions to accept datasets/gene lists from any species. 
Now automatically converted to `output_species` (default: "human") using [`orthogene`](https://github.com/neurogenomics/orthogene). 
* Create MAGMA files repository using various OpenGWAS datasets 
that have been munged with `MungeSumstats`: [https://github.com/neurogenomics/MAGMA_Files_Public](MAGMA_Files_Public)
* Added API to search and acesss MAGMA files repository: `import_magma_files`.
* Allow all relevant functions to take only MAGMA files as input 
(instead of requiring the GWAS summary stats); e.g. `calculate_celltype_associations(magma_dir="<folder_containing_magma_files>")`
This function is also used for downloading MAGMA files in examples/unit tests.
* Add header notation in code comments to improve code navigability.
* Fix Roxygen notes:
    - Document `@title`,`@description`,`@param`, `@return` 
    for all exported (and many internal) functions.
    - Document  `@examples` 
    for all exported (and many internal) functions.
    - Used `@importFrom` or `requireNamespace` for all imports functions.
* Replace usage of all `1:10` syntax.
* Reduce number of functions in NAMESPACE
* Set all defaults consistently across all functions: 
    - `upstream_kb = 35`
    - `downstream_kb = 10`
* Allow the use of non-European populations by downloading 
population-specific LD panels from 1KG with 
`get_genome_ref(population = "<population_name>")` 
* Handle other CTD matrix input types by ensuring standardisation 
as dense matrices when computing quantiles/normalization. 
* Take advantage of new `EWCE` features in *bschilder_dev* branch:
    - Standardise CTD internally in all relevant functions using new
`EWCE::standardise_ctd`
* Create all-in-one functions `celltype_associations_pipeline`, 
which lets users specify which test they want to run with arguments, including:
    - `calculate_celltype_associations` (Linear mode)
    - `calculate_celltype_associations` (Top10% mode)
    - `calculate_conditional_celltype_associations`
* Parallelise `celltype_associations_pipeline` across multiple cores. 


## Bug fixes 

* Removed `usethis` call from code. 
* Removed all `library` calls from code.
* Avoid accidentally renaming columns with `data.frame` 
* Remove all `suppressWarnings` calls and resolve the underlying issues instead. 
* Add `utils` as Suggest.
* Normalize paths to magma executables (to avoid path issues on WindowsOS).
