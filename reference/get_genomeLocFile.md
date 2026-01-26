# Import gene location files

Imports a file gene coordinate from its respective genome build.

## Usage

``` r
get_genomeLocFile(
  build,
  storage_dir = tools::R_user_dir("MAGMA.Celltyping", which = "cache"),
  overwrite = FALSE
)
```

## Source

``  ## Move from data/ to Releases on GitHub to speed up MAGMA.Celltyping prefixes <- c("NCBI36.3","NCBI37.3","NCBI38") ### Made a permalink so we can always go back to this version of the data ### (even after it's been deleted from the current version). permalink_id <- "f931877dedcde103723f14242d5242fdca7b3af6" files <- stats::setNames( paste("https://github.com/neurogenomics/MAGMA_Celltyping/raw", permalink_id,"data", paste0(prefixes,".gene.loc"), sep="/"), prefixes ) local_files <- lapply(names(files), function(x){ local <- file.path(tempdir(),basename(files[x])) utils::download.file(files[x],local) return(local) }) |> `names<-`(names(files))for(x in names(local_files)){ piggyback::pb_upload(file = local_files[[x]], repo = "neurogenomics/MAGMA_Celltyping", overwrite = TRUE) }  ``

` tmp <- MAGMA.Celltyping:::get_genomeLocFile(build = "GRCH37") `

## Arguments

- build:

  Genome build. Must be one of:

  - "GRCH36" : Corresponds to file "NCBI36.3.gene.loc"

  - "GRCH37" : Corresponds to file "NCBI37.3.gene.loc"

  - "GRCH38" : Corresponds to file "NCBI38.gene.loc"

- storage_dir:

  Folder in which to save the file.

- overwrite:

  Should any local files of the same name be overwritten? default
  `TRUE`.

## Value

File path.
