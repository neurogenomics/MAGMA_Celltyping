# Map SNPs to their nearby genes

Make two external calls to MAGMA. First use it to annotate SNPs onto
their neighbouring genes. Second, use it to calculate the gene level
trait association.

## Usage

``` r
map_snps_to_genes(
  path_formatted,
  genome_build = NULL,
  upstream_kb = 35,
  downstream_kb = 10,
  N = NULL,
  duplicate = c("drop", "first", "last", "error"),
  synonym_dup = c("skip", "skip-dup", "drop", "drop-dup", "error"),
  genome_ref_path = NULL,
  population = "eur",
  genes_only = FALSE,
  storage_dir = tools::R_user_dir("MAGMA.Celltyping", which = "cache"),
  force_new = FALSE,
  version = NULL,
  verbose = TRUE
)
```

## Arguments

- path_formatted:

  Filepath of the summary statistics file (which is expected to already
  be in the required format). Can be uncompressed or compressed (".gz"
  or ".bgz").

- genome_build:

  The build of the reference genome (`"GRCh37"` or `"GRCh38"`). If
  `NULL`, it will be inferred with
  [get_genome_build](https://al-murphy.github.io/MungeSumstats/reference/get_genome_build.html).

- upstream_kb:

  How many kilobases (kb) upstream of the gene should SNPs be included?

- downstream_kb:

  How many kilobases (kb) downstream of the gene should SNPs be
  included?

- N:

  What is the N number for this GWAS? That is cases + controls.

- duplicate:

  The duplicate modifier can be used to specify the desired behaviour
  for dealing with duplicate SNPs in the file, and can be set to one of
  four values: 'drop', 'first', 'last', and 'error'. When set to 'drop',
  the corresponding SNP is removed from the analysis entirely. When set
  to 'first' or 'last', either the first or the last entry for that SNPs
  in the file is used. When set to 'error', the program terminates if
  encountering any duplicate SNPs. The default mode is 'duplicate=drop'.
  Note that SNPs are only checked for duplication if they are present in
  the genotype data, and if they have a non-missing pvalue (and sample
  size, if ncol is set). When synonymous SNP IDs have been loaded,
  different SNP IDs referring to the same SNP are considered duplicates
  as well. Unless duplicate is set to 'error', a list of duplicate SNPs
  will be written to the supplementary log file.

- synonym_dup:

  When loading SNP ID synonyms, MAGMA may detect SNP IDs in the genotype
  data that are synonyms of each other. The synonym-dup modifier for the
  –bfile flag can be used to specify the desired behaviour for dealing
  with such SNPs. This modifier can be set to one of four values:
  'drop', 'drop-dup', 'skip', 'skip-dup' and 'error'. When set to
  'drop', SNPs that have multiple synonyms in the data are removed from
  the analysis. Conversely, when set to 'skip' the SNPs are left in the
  data and the synonym entry in the synonym file is skipped. When set to
  'drop-dup', for each synonym entry only the first listed in the
  synonym file is retained; for subsequent SNP IDs in the same entry
  that are found in the data are removed, and their IDs are mapped as
  synonyms to the first SNP. When set to 'skipdup' the genotype data for
  all synonymous SNPs is retained; SNP IDs not found in the data are
  mapped to the first SNP in the synonym entry that is. Finally, when
  set to 'error', the program will simply terminate when encountering
  synonymous SNPs in the data. The default mode is 'synonym-dup=skip'.
  Unless synonym-dup is set to error, a list of synonymous SNPs in the
  data will be written to the supplementary log file.

- genome_ref_path:

  Path to the folder containing the 1000 genomes reference (downloaded
  with
  [get_genome_ref](https://neurogenomics.github.io/MAGMA_Celltyping/reference/get_genome_ref.md)).

- population:

  Which population subset of the genome reference to include.

  - "eur" : European descent (Default simply because this is currently
    the most common GWAS subpopulation).

  - "afr" : African descent.

  - "amr" : Ad Mixed American descent.

  - "eas" : East Asian descent.

  - "sas" : South Asian descent.

- genes_only:

  The *.genes.raw* file is the intermediary file that serves as the
  input for subsequent gene-level analyses. To perform only a gene
  analysis, with no subsequent gene-set analysis, the `--genes-only`
  flag can be added (`TRUE`). This suppresses the creation of the
  *.genes.raw* file, and significantly reduces the running time and
  memory required.

- storage_dir:

  Where to store genome ref.

- force_new:

  Set to `TRUE` to rerun `MAGMA` even if the output files already exist.
  (Default: `FALSE`).

- version:

  MAGMA version to use.

- verbose:

  Print messages.

## Value

Path to the genes.out file.

## Examples

``` r
if (FALSE) { # \dontrun{
path_formatted <- MAGMA.Celltyping::get_example_gwas()
genesOutPath <- MAGMA.Celltyping::map_snps_to_genes(
    path_formatted = path_formatted,
    genome_build = "hg19",
    N = 5000)
} # } 
```
