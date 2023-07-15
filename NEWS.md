# MAGMA.Celltyping 2.0.11

## Bug fixes

- `create_fake_gwas_path`
  - Don't remove "MAGMA_Files" from all parts of path.

# MAGMA.Celltyping 2.0.10

## New features

* `map_snps_to_genes` 
  - Add new `duplicate` arg to control what to do with duplicate/non-bilalleic SNPs.
  - Add new `synonym_dup` arg to control what to do with synonymous SNPs.
  - Add subfunctions `check_duplicate`, `check_synonym_dup`
* Add bug/feature report note to startup message.

## Bug fixes

* `get_magma_paths`
  - Uses `file.path` instead of hard-coded "/" symbols.
* `map_snps_to_genes` --> 
  - `decompress` wasn't recognizing files with ".bgz" suffix. 

# MAGMA.Celltyping 2.0.9

## New features  

* Update tags from "latest" to "v2.0.0" and "v2.0.8"
    - Did this so I can better version-control each Release. 
    Plus I'm renaming some CTDs to make them more intuitive.
    - Also adding some new CTDS: "TabulaMuris_droplet", "TabulaMuris_facs"
* `get_data` / `get_ctd`
    - Expose `.token` arg.
    - Expose `tag` arg.
    - Make token error instructions more explicit and add GH docs link.

# MAGMA.Celltyping 2.0.8

## New features  

* Add `rworkflows`
    - GHA
    - Replace `badger`
* `get_ctd_dendro`
    - Automatically compute spec quantiles matrix when its missing.
* `plot_celltype_associations`
    - Replace `sprintf`, `for`, 
    - Control verbosity
    - Replace grid with `patchwork`
    - Save with ` ggplot2::ggsave` instead of `grDevices`
    - Reduce redundancy in code (e.g. saving plots)
    - Add new arg to bind plots from multiple levels into one: `bind_plots`
* `enrichment_results`
    - Update example enrichment results to new format with nested `levels`.
* Add *CITATION* file.
* Add citation info and volcano/cell icons to startup message.
* Replace `%>%` with `|>` syntax.
* `celltype_associations_pipeline`:
    - Print error messages.
* `get_ctd`
    - Add new *ctd_allAIBS*: https://github.com/neurogenomics/MAGMA_Celltyping/issues
* `map_snps_to_genes`
    - Fix `genesOut` path for Windows.
    - Subfunctionalize :`check_n`, `check_genomeLocFile`
* `enrichment_results`:
    - Update example results with extra slots and named levels.
* Added raw MAGMA results file as an example:
    - "inst/extdata/ieu-a-298.tsv.gz.35UP.10DOWN.level1.MainRun.gsa.out"
* `calculate_conditional_celltype_associations`
    - Pass up new arg: `qvalue_thresh`
    - Add unit tests that test for non-NULL results.
    - Ensure the BASELINE
* *test-celltype_associations_pipeline.r*
    - Add unit tests for additional CTDs (e.g. "ctd_AIBS","ctd_Aerts2018").
* `magma_tileplot`
    - Replace  `grDevices` / `gridExtra` with `patchwork`
    - Move legend to top
    - Add axis labels
    - Add unit tests
* Add new function `fix_celltype_names2`
    for situations when you don't want to make all celltypes in vector unique.
* Add unit tests for interactive `check_n` func
    
## Bug fixes 

* `plot_celltype_associations`
    - Fix issues described in PR: https://github.com/neurogenomics/MAGMA_Celltyping/pull/121
    - Fix NAs issue by ensuring celltype names in `ctAssocs` and `ctdDendro`
        are both run through `EWCE::fix_celltype_names()` 
        when creating an ordered factor.
* `merge_magma_results`
    - Add names to levels 
* `calculate_celltype_associations`:
    - Fix issue raised here: https://github.com/neurogenomics/MAGMA_Celltyping/pull/135
    - Made flags conditional on MAGMA version.
    - Subfunctionalize: `calculate_celltype_associations_linear`, `calculate_celltype_associations_top10`
    - Ensure `EnrichmentMode` gets added to the saved file names to distinguish results from one another
        via new internal func: `check_analysis_name`.
* `load_magma_results_file`
    - Was incorrectly giving *>50% of celltypes missing. %s celltypes* error whenever Top10% + 
        conditional was being used.
    - Remove creations of `res$COVAR` column, as this is no longer relevant 
        given the structure of the results table.
    - Remove outdated steps to fix celltype names, which were very roundabout and convoluted.
    - Don't set `rownames(res) <- res$VARIABLE` as this causes duplicate rownames.
    - Now exported
    - Add new column "analysis_name"
    - Add new arg: `keep_types=` to subset results types.
* `map_snps_to_genes`
    - Fix bug preventing automatic detection of "N" column.


# MAGMA.Celltyping 2.0.7

## New features  

* `plot_celltype_associations`: added unit tests.
* `calculate_conditional_celltype_associations`: added unit tests. 
    - Also runs `calculate_celltype_associations` internally. 

## Bug fixes 

* Get GitHub Actions to pass on all 3 OS. 

# MAGMA.Celltyping 2.0.6

## New features

* `import_magma_files`:
    - Add args to import as nested list and/or `data.table`s. 
    - Add dedicated unit tests. 
* `magma_files_metadata`
    - Update after deleting old files. 
* `celltype_associations_pipeline`:
    - Pass up `standardise` arg.
* `adjust_zstat_in_genesOut`:
    - Enable usage without CTD.
    - Add unit tests.
* Infer `ctd` and `geneset` species by default:
    - `infer_ctd_species`
    - `infer_geneset_species`
* `map_snps_to_genes`:
    - Can interpret `N` when `=NULL` or `=NA`.
    - Added `genes_only` arg to make function faster and 
    skip generating "genes.raw" file.  

## Bug fixes 

* Ensure all functions work with sparse matrices.
* `check_quantiles`:
    - Compute "specificity_quantiles" if not already present in CTD 
    (workaround for bug in `EWCE::standardise_ctd`). 
* `decompress`
    - Specify file path as `storage_dir` instead of `tempdir()`. 
* Update `full_workflow` vignette to work.
* `magma_tileplot`:
    - Fix to handle situations where all q-values equal 1. 
* `get_example_gwas`:
    - Revert default storage dir to `tempdir()` 
    to make easier to infer MAGMA output file paths in vignettes. 
* `merge_results`: 
    - Create`save_dir` if not already present.
* `get_driver_genes`:
    - Fix bug that searched for "hgnc.symbol" instead of "hgnc_symbol". 
    - Sort by mean rank between "ADJ_ZSTAT" and "specificity_proportion".  
* `magma_files_metadata`:
    - Updated to reflect newly reprocessed 288 datasets. 
    - Reprocessed "ukb-b-6548" to get `calculate_conditional_celltype_enrichment_limma` example working again. 
    
# MAGMA.Celltyping 2.0.5

## New features

* `get_ctd`:
    - Documented new CTDs from model celltype conservation project.  
    - Added better documentation for each of the other CTDs as well (including references).
    - Re-standardized all CTD with updated `EWCE::standardise_ctd` function.
    - Enabled download of multiple CTDs in one call. 
    - Cached all CTDs (not in `tempdir`). 
    - Import either RDS or RDA files. 

# MAGMA.Celltyping 2.0.4

## Bug fixes

* Fix GHA pkgdown building: 
    - The newest version of [git introduced bugs when building pkgdown sites](https://github.com/actions/checkout/issues/760) 
    from within Docker containers (e.g. via my Linux GHA workflow). 
    Adjusting GHA to fix this. 

# MAGMA.Celltyping 2.0.3

## Bug fixes

* Ensure `get_genome_ref` saves to cache dir (not temp dir)
during `map_snps_to_genes`. 
* Fix GHA release/devel conflicts. 
* Add optional error handling with GitHub PAT fix instructions to `get_data`, 
specifically `get_genomeLocFile`. 
* Construct URLs with `paste` instead of `file.path` 
to avoid cross-platforms issues (e.g. Windows inserting "\" instead of "/"). 
* Add `dir.create` to `get_data`.  

# MAGMA.Celltyping 2.0.2

## New features

* `get_genomeLocFile`: Store NCBI ref file in cache dir by default.  

## Bug fixes  

* Add windows-specific files/folders to *.Rbuildignore*.  
* Bump *Depends* to R(>= 4.1).  

# MAGMA.Celltyping 2.0.1  

## New features

* `get_example_gwas_raw`: Allow users to specify `timeout` when downloading 
raw (pre-munged) files. 
* Added [Docker/Singularity](https://neurogenomics.github.io/MAGMA_Celltyping/articles/docker) vignette, with link in README.  
* Add *--as-cran* to R CMD check in GHA. 

## Bug fixes  

* Add `$annot` data.frames to each ctd in `get_ctd` to make compatible with all `EWCE` functions.  
* Add "ctd_DescartesHuman" to documented datasets in `get_ctd`.  

# MAGMA.Celltyping 2.0.0 

* MAJOR UPGRADE: `MAGMA.Celltyping` was revamped to meet CRAN standards,
automatically install MAGMA, and take any species are input.

## New features

* Added a `NEWS.md` file to track changes to the package.
* Automatically install MAGMA with new `magma_install` function; 
stores binaries in `MAGMA.Celltyping`-specific cache dir. Added various support functions to make this possible and ensure correct version is being used.
* Added `magma_uninstall` function to remove one or all MAGMA binaries. 
* Allow `MAGMA.Celltyping` to install even if MAGMA is not installed. Instead,
check at the beginning of functions that require MAGMA using `magma_check`.
    - `magma_links_stored`: Include built-in metadata with links to all MAGMA 
    versions with parsed version numbers, OS, and which is the latest version. 
* Call MAGMA commands with `magma_run` which finds the requested version of 
MAGMA and uses it. 
* Print readable, user-friendly MAGMA commands when being run through
`magma_cmd` function. 
* Added unit tests.
* Create hex sticker
* New function: `get_sub_SNP_LOC_DATA`
* Formally deprecated functions using `.Deprecated` function and removing all other internal code:
    - `get_genomebuild_for_sumstats`
    - `build_snp_location_tables`
    - `format.sumstats.for.magma`
    - `format_sumstats_for_magma_macOnly`
    - `standardise.sumstats.column.headers`
    - `standardise.sumstats.column.headers.crossplatform`
* Removed `sumstatsColHeaders` from data, as it was only used in now-deprecated 
functions.  
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
* Remove unnecessary dependencies:
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
that have been munged with `MungeSumstats`: [MAGMA_Files_Public](https://github.com/neurogenomics/MAGMA_Files_Public)
    - `magma_files_metadata`: Built-in table of all pre-processed MAGMA files
    currently in the database.  
* Added API to search and access MAGMA files repository: `import_magma_files`.
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
* Removed old functions whose output were not being used: 
    - `normalise_mean_exp`
    - `bin_specificityDistance_into_quantiles`
    - `bin_expression_into_quantiles` 
* Add new function (plus tests):  
    - `get_driver_genes`  
* Added unit tests for: 
    - `calculate_celltype_enrichment_limma` 
    - `adjust_zstat_in_genesOut` 
    - Deprecated functions 
* Added R script to produce vignette results *inst/extdata/MAGMA_Celltyping_1.0_vignette.R*, and uploaded zipped folder via piggyback: *MAGMA_Celltyping_1.0_results.zip*  
* Added unit tests comparing old (1.0.0) vs new (>=2.0) MAGMA.Celltyping versions produce the same results; *test-MAGMA_Celltyping_1.0_vs_2.0.R*. Full report [here](https://github.com/neurogenomics/MAGMA_Celltyping/issues/96).  

## Bug fixes 

* Removed `usethis` call from code. 
* Removed all `library` calls from code.
* Avoid accidentally renaming columns with `data.frame` 
* Remove all `suppressWarnings` calls and resolve the underlying issues instead. 
* Add `utils` as Suggest.
* Normalize paths to magma executables (to avoid path issues on WindowsOS).
* Fixed axes in `plot_celltype_associations`, first reported [here](https://github.com/neurogenomics/MAGMA_Celltyping/issues/12).  
* Fixed `prepare_quantile_groups` so that it's consistent with how `EWCE` 
compute specificity quantiles. Ensures that all celltypes (columns) 
have exactly the same number of quantiles, which was not the case before. 
* Fixed bug in ``
