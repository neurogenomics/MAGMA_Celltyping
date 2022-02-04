<img src='https://github.com/neurogenomics/MAGMA_Celltyping/raw/master/inst/hex/hex.png' height='300'><br><br>
[![](https://img.shields.io/badge/devel%20version-2.0.1-black.svg)](https://github.com/neurogenomics/MAGMA_Celltyping)
[![R build
status](https://github.com/neurogenomics/MAGMA_Celltyping/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/neurogenomics/MAGMA_Celltyping/actions)
[![](https://img.shields.io/github/last-commit/neurogenomics/MAGMA_Celltyping.svg)](https://github.com/neurogenomics/MAGMA_Celltyping/commits/master)
[![](https://codecov.io/gh/neurogenomics/MAGMA_Celltyping/branch/master/graph/badge.svg)](https://codecov.io/gh/neurogenomics/MAGMA_Celltyping)
[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
<h4>
Authors: <i>Brian Schilder, Alan Murphy, Julien Bryois, Nathan Skene</i>
</h4>
<h4>
README updated: <i>Feb-04-2022</i>
</h4>

## Introduction

This R package contains code used for testing which cell types can
explain the heritability signal from GWAS summary statistics. The method
was described in our [2018 Nature Genetics
paper](https://www.nature.com/articles/s41588-018-0129-5).

This package takes GWAS summary statistics + single-cell transcriptome
specificity data (in [EWCE](https://github.com/NathanSkene/EWCE)’s
CellTypeData format) as input. It then calculates and returns the
enrichment between the GWAS trait and the cell-types.

## Installation

### R

Install `MAGMA.Celltyping` as follows:

``` r
if(!require("remotes")) install.packages("remotes")

remotes::install_github("neurogenomics/MAGMA_Celltyping")
library(MAGMA.Celltyping)
```

### MAGMA

`MAGMA.Celltyping` now installs the command line software MAGMA
automatically when you first use a function that relies on MAGMA
(e.g. `celltype_associations_pipeline`). If you prefer, you can later
install other versions of MAGMA with:

``` r
MAGMA.Celltyping::install_magma(desired_version="<version>",
                                update = TRUE)
```

## Documentation

### [Website](https://neurogenomics.github.io/MAGMA_Celltyping)

### [Getting started](https://neurogenomics.github.io/MAGMA_Celltyping/articles/MAGMA_Celltyping)

### [Docker/Singularity](https://neurogenomics.github.io/MAGMA_Celltyping/articles/docker)

## Using older versions

With the release of `MAGMA_Celltyping` 2.0 in January 2022, there have
been a number of [major updates and bug
fixes](https://github.com/neurogenomics/MAGMA_Celltyping/pull/93).

-   Only R&gt;4.0.0 is supported. To use this package with older
    versions of R, install
    with:`remotes::install_github("neurogenomics/MAGMA_Celltyping@01a9e53")`

## Bugs/fixes

Having trouble? Search the
[Issues](https://neurogenomics.github.io/MAGMA_Celltyping/issues) or
submit a new one.

Want to contribute new features/fixes? [Pull
Requests](https://neurogenomics.github.io/MAGMA_Celltyping/pulls) are
welcomed!

Both are most welcome, we want the package to be easy to use for
everyone!

## Citations

If you use the software then please cite:

> [Skene, et al. Genetic identification of brain cell types underlying
> schizophrenia. Nature Genetics,
> 2018.](https://www.nature.com/articles/s41588-018-0129-5)

The package utilises the MAGMA software developed in the Complex Trait
Genetics Lab at VU university (not us!) so please also cite:

> [de Leeuw, et al. MAGMA: Generalized gene-set analysis of GWAS data.
> PLoS Comput Biol,
> 2015.](https://journals.plos.org/ploscompbiol/article?id=10.1371%2Fjournal.pcbi.1004219)

If you use the EWCE package as well then please cite:

> [Skene, et al. Identification of Vulnerable Cell Types in Major Brain
> Disorders Using Single Cell Transcriptomes and Expression Weighted
> Cell Type Enrichment. Front. Neurosci,
> 2016.](https://www.frontiersin.org/articles/10.3389/fnins.2016.00016/full)

If you use `MungeSumstats` to format your summary statistics then please
cite:

> [Murphy, Schilder, & Skene, MungeSumstats: a Bioconductor package for
> the standardization and quality control of many GWAS summary
> statistics, Bioinformatics, Volume 37, Issue 23, 1 December 2021,
> Pages 4593–4596,
> https://doi.org/10.1093/bioinformatics/btab665](https://doi.org/10.1093/bioinformatics/btab665)

If you use the cortex/hippocampus single cell data associated with this
package then please cite the following papers:

> [Zeisel, et al. Cell types in the mouse cortex and hippocampus
> revealed by single-cell RNA-seq.Science,
> 2015.](http://www.sciencemag.org/content/early/2015/02/18/science.aaa1934.abstract)

If you use the midbrain and hypothalamus single cell datasets associated
with the 2018 paper then please cite the following papers:

> [La Manno, et al. Molecular Diversity of Midbrain Development in
> Mouse, Human, and Stem Cells. Cell,
> 2016.](http://www.cell.com/cell/fulltext/S0092-8674(16)31309-5)

> [Romanov, et al. Molecular interrogation of hypothalamic organization
> reveals distinct dopamine neuronal subtypes. Nature Neuroscience,
> 2016.](http://www.nature.com/neuro/journal/vaop/ncurrent/full/nn.4462.html)

<hr>

## Contact

### [Neurogenomics Lab](https://www.neurogenomics.co.uk/)

UK Dementia Research Institute  
Department of Brain Sciences  
Faculty of Medicine  
Imperial College London  
[GitHub](https://github.com/neurogenomics)  
[DockerHub](https://hub.docker.com/orgs/neurogenomicslab)

<br>
