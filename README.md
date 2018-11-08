# README #

This R package contains code neccesary for testing which cell types can explain the heritability signal from GWAS summary statistics. The method was described in our 2018 Nature Genetics paper, *"Genetic identification of brain cell types underlying schizophrenia"*. 

### How do I get set up? ###

Before installing this package it is neccesary to install the magma software package. Please download it from https://ctg.cncr.nl/software/magma. The executable should then be copied to /usr/local/bin so that R can find it. Then install this package as follows:

```
install.packages("devtools")
library(devtools)
install_github("nathanskene/ewce")
install_github("nathanskene/MAGMA_Celltyping")
```

There is a vignette describing how to use the package located in /vignettes/introduction.Rmd

### Where do I download the dataset described in the paper?

This package provides a smaller version of the Karolinska cell type dataset. The full Karolinska and AIBS datasets as described in the 2018 paper can be downloaded from http://www.hjerling-leffler-lab.org/data/scz_singlecell/

### Who do I talk to? ###

If you have any issues using the package then please get in touch with Nathan Skene (nathan.skene at ki.se). Bug reports etc are all most welcome, we want the package to be easy to use for everyone!

### Citation

If you use the software then please cite

[Skene, et al. Genetic identification of brain cell types underlying schizophrenia.
Nature Genetics, 2018.](https://www.nature.com/articles/s41588-018-0129-5)

The package builds on the MAGMA package developed in the Complex Trait Genetics lab at VU university (not us!) so please also cite their work:

[de Leeuw, et al. MAGMA: Generalized gene-set analysis of GWAS data.
PLoS Comput Biol, 2015.](https://journals.plos.org/ploscompbiol/article?id=10.1371%2Fjournal.pcbi.1004219)

If you use the EWCE package as well then please cite

[Skene, et al. Identification of Vulnerable Cell Types in Major Brain Disorders Using Single Cell Transcriptomes and Expression Weighted Cell Type Enrichment.
Front. Neurosci, 2016.](https://www.frontiersin.org/articles/10.3389/fnins.2016.00016/full)