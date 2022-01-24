#### 0. Install MAGMA v1.06 ####
## Download and unzip file in command line, outside of Docker container where 
## MAGMA.Celltyping v1.0.0 will be installed.
# wget https://ctg.cncr.nl/software/MAGMA/prog/archive/magma_v1.06.zip
# unzip magma_v1.06.zip

## Find your container name, under the NAME column
# docker container ls
# docker cp magma_v1.06/magma <container_name>:/bin/magma

## Make sure you give yourself execution permissions when you're using it within the Docker container
# docker exec -t -i <container_name> chmod +x /bin/magma 

#### 1. Install old version of MAGMA.Celltyping (v1.0.0) ####

## https://github.com/neurogenomics/MAGMA_Celltyping/tree/01a9e53ed555fa910dad8854100535ed9eecf3a1
remotes::install_github("neurogenomics/MAGMA_Celltyping@01a9e53ed555fa910dad8854100535ed9eecf3a1")
## https://github.com/NathanSkene/EWCE/tree/5b89b57c70a58bf37391983c8c4e67678c5e01b8
remotes::install_github("NathanSkene/EWCE@5b89b57c70a58bf37391983c8c4e67678c5e01b8")
remotes::install_github("NathanSkene/One2One")
library(MAGMA.Celltyping)
library(One2One)
library(EWCE)
library(R.utils)


#### 2. Follow vignette in README from v1.0.0  to reproduce results ####
storage_dir <- "/Desktop/MAGMA_Celltyping_1.0_results"
## Folder to store all results that will be uploaded to GH Resources 
dir.create(storage_dir) 

# Set path the 1000 genomes reference data.
genome_ref_dir = file.path(storage_dir,"g1000_eur")
if(!file.exists(sprintf("%s/g1000_eur.bed",genome_ref_dir))){
    download.file("https://ctg.cncr.nl/software/MAGMA/ref_data/g1000_eur.zip",
                  destfile=sprintf("%s.zip",genome_ref_dir))
    unzip(sprintf("%s.zip",genome_ref_dir),exdir=genome_ref_dir)
}
genome_ref_path = sprintf("%s/g1000_eur",genome_ref_dir)
# Load the celltype data
ctd <- ewceData::ctd()
# Load the mouse to human 1:1 orthologs from the One2One package
data(ortholog_data_Mouse_Human)
ctd = prepare.quantile.groups(ctd,specificity_species="mouse",numberOfBins=40)


# Download and unzip the summary statistics file

{
    gwas_sumstats_path = file.path(storage_dir,"GWAS","20016_irnt.gwas.imputed_v3.both_sexes.tsv")
    gwas_sumstats_path_formatted =sprintf("%s.formatted.tsv",gsub(".tsv$","",gwas_sumstats_path))
    dir.create(dirname(gwas_sumstats_path))
    if(!file.exists(gwas_sumstats_path)){
        #download.file("https://www.dropbox.com/s/shsiq0brkax886j/20016.assoc.tsv.gz?raw=1",destfile=sprintf("%s.gz",gwas_sumstats_path))
        download.file("https://www.dropbox.com/s/t3lrfj1id8133sx/20016_irnt.gwas.imputed_v3.both_sexes.tsv.bgz?dl=1",
                      destfile=sprintf("%s.gz",gwas_sumstats_path))
        gunzip(sprintf("%s.gz",gwas_sumstats_path),gwas_sumstats_path)
    } 
    # Format & map SNPs to genes
    tmpSumStatsPath = MAGMA.Celltyping::format_sumstats_for_magma(gwas_sumstats_path) 
    file.copy(from=tmpSumStatsPath,to=gwas_sumstats_path_formatted,overwrite = TRUE) 
    genesOutPath = map.snps.to.genes(path_formatted=gwas_sumstats_path_formatted,
                                     genome_ref_path=genome_ref_path)
}

### Run the main cell type association analysis
ctAssocsLinear = calculate_celltype_associations(ctd=ctd,
                                                 gwas_sumstats_path=gwas_sumstats_path_formatted,
                                                 genome_ref_path=genome_ref_path,
                                                 specificity_species = "mouse")
saveRDS(ctAssocsLinear, file.path(storage_dir,"ctAssocsLinear.rds"))
# FigsLinear = plot_celltype_associations(ctAssocs=ctAssocsLinear,
#                                         ctd=ctd)
# Now let's add the top 10% mode
ctAssocsTop = calculate_celltype_associations(ctd=ctd,
                                              gwas_sumstats_path=gwas_sumstats_path_formatted,
                                              genome_ref_path=genome_ref_path,
                                              EnrichmentMode="Top 10%")
saveRDS(ctAssocsTop, file.path(storage_dir,"ctAssocsTop.rds"))
# FigsTopDecile = plot_celltype_associations(ctAssocs=ctAssocsTop,
#                                            ctd=ctd)

# Then plot linear together with the top decile mode
ctAssocMerged = merge_magma_results(ctAssoc1=ctAssocsLinear,
                                    ctAssoc2=ctAssocsTop)
saveRDS(ctAssocMerged, file.path(storage_dir,"ctAssocMerged.rds"))
# FigsMerged = plot_celltype_associations(ctAssocs=ctAssocMerged,
#                                         ctd=ctd)



### Run the conditional cell type association analysis (linear mode)

# By default, it is assumed that you want to run the linear enrichment analysis. There are two modes for conditional analyses, you can either control for the top N cell types from the baseline analysis (in which case, set controlTopNcells) or control for specific specified cell types (in which case, set controlledCTs).

# Conditional analysis
ctCondAssocs1 = calculate_conditional_celltype_associations(ctd,gwas_sumstats_path_formatted,genome_ref_path=genome_ref_path,
                                                            analysis_name = "Conditional",
                                                            controlTopNcells=2)
saveRDS(ctCondAssocs1, file.path(storage_dir,"ctCondAssocs_controlTopNcells-2.rds"))
# plot_celltype_associations(ctCondAssocs,ctd=ctd)

# Let's try as an alternative to control for expression of both the level 1 pyramidal neuron types at the same time
controlledCTs <- sort(c("pyramidal CA1","pyramidal SS","interneurons"))
ctCondAssocs = calculate_conditional_celltype_associations(ctd,gwas_sumstats_path_formatted,genome_ref_path=genome_ref_path,
                                                           analysis_name = "Conditional",
                                                           controlledCTs=controlledCTs,
                                                           controlledAnnotLevel=1)
ct_filename <- paste(gsub(" ",".",controlledCTs),collapse = ",")
saveRDS(ctCondAssocs, file.path(storage_dir,paste0("ctCondAssocs_controlledCTs-",ct_filename,".rds")))
# plot_celltype_associations(ctCondAssocs,ctd=ctd)


# Note that Periventricular Microglia (PVM) go from totally non-significant to significant once the neurons are controlled for. Test if this change is significant as follows:
magma1 = ctCondAssocs[[1]]$results[ctCondAssocs[[1]]$results$CONTROL=="BASELINE",]
magma2 = ctCondAssocs[[1]]$results[ctCondAssocs[[1]]$results$CONTROL==ct_filename,]
resCompared = compare.trait.enrichments(magma1=magma1,magma2=magma2,annotLevel=2,ctd=ctd)
saveRDS(resCompared, file.path(storage_dir,"resCompared.rds"))



## Conditional analyses (top 10% mode)

# Conditional analyses can also be performed with top 10% mode (although the conditioning is done in linear mode)
ctCondAssocsTopTen = calculate_conditional_celltype_associations(ctd,gwas_sumstats_path_formatted,genome_ref_path=genome_ref_path,
                                                                 analysis_name = "Conditional",
                                                                 controlledCTs=controlledCTs,
                                                                 controlledAnnotLevel=1,
                                                                 EnrichmentMode = "Top 10%")
saveRDS(ctCondAssocsTopTen, file.path(storage_dir,"rctCondAssocsTopTen.rds"))
# plot_celltype_associations(ctCondAssocsTopTen,ctd=ctd)





## Controlling for a second GWAS

# We now want to test enrichments that remain in a GWAS after we control for a second GWAS. So let's download a second GWAS sumstats file and prepare it for analysis.
# 
# 20018.assoc.tsv is the sumstats file for 'Prospective memory result' from the UK Biobank. 
# 
# 20016.assoc.tsv is the sumstats file for 'Fluid Intelligence Score' from the UK Biobank. 
# 
# So let's subtract genes associated with prospective memory from those involved in fluid intelligence.

### Download and prepare the 'Prospective memory' GWAS summary statistics

{
    # Download and unzip the summary statistics file
    gwas_sumstats_path = file.path(storage_dir,"GWAS","20018.gwas.imputed_v3.both_sexes.tsv")
    gwas_sumstats_path_formatted = sprintf("%s.formatted.tsv",gsub(".tsv$","",gwas_sumstats_path))
    dir.create(dirname(gwas_sumstats_path))
    if(!file.exists(gwas_sumstats_path)){
        download.file("https://www.dropbox.com/s/j6mde051pl8k8vu/20018.gwas.imputed_v3.both_sexes.tsv.bgz?dl=1",
                      destfile=sprintf("%s.gz",gwas_sumstats_path))
        gunzip(sprintf("%s.gz",gwas_sumstats_path),gwas_sumstats_path)
    }
    # Format & map SNPs to genes
    tmpSumStatsPath = format_sumstats_for_magma(gwas_sumstats_path) 
    file.copy(from=tmpSumStatsPath,to=gwas_sumstats_path_formatted,overwrite = TRUE)
    genesOutPath = map.snps.to.genes(gwas_sumstats_path_formatted,
                                     genome_ref_path=genome_ref_path)
}

### Check which cell types this GWAS is associated with at baseline
gwas_sumstats_path_Memory = file.path(storage_dir,"GWAS","20018.gwas.imputed_v3.both_sexes.formatted.tsv")
gwas_sumstats_path_Intelligence = file.path(storage_dir,"GWAS","20016_irnt.gwas.imputed_v3.both_sexes.formatted.tsv")
ctAssocsLinearMemory = calculate_celltype_associations(ctd,gwas_sumstats_path_Memory,genome_ref_path=genome_ref_path,
                                                       specificity_species = "mouse")
ctAssocsLinearIntelligence = calculate_celltype_associations(ctd,gwas_sumstats_path_Intelligence,genome_ref_path=genome_ref_path,
                                                             specificity_species = "mouse")
saveRDS(ctAssocsLinearMemory, file.path(storage_dir,"ctAssocsLinearMemory.rds"))
saveRDS(ctAssocsLinearIntelligence, file.path(storage_dir,"ctAssocsLinearIntelligence.rds"))
# plot_celltype_associations(ctAssocsLinearMemory,ctd=ctd)


### Compare enrichments in the two GWAS using a tile plot

ctAssocMerged_MemInt = merge_magma_results(ctAssocsLinearMemory,ctAssocsLinearIntelligence)
saveRDS(ctAssocMerged_MemInt, file.path(storage_dir,"ctAssocMerged_MemInt.rds"))
# FigsMerged_MemInt = magma.tileplot(ctd=ctd,results=ctAssocMerged_MemInt[[1]]$results,annotLevel=1,fileTag="Merged_MemInt_lvl1",output_path = "~/Desktop")
# FigsMerged_MemInt = magma.tileplot(ctd=ctd,results=ctAssocMerged_MemInt[[2]]$results,annotLevel=2,fileTag="Merged_MemInt_lvl2",output_path = "~/Desktop")


### Check which cell types 'Fluid Intelligence' is associated with after controlling for 'Prospective memory'

# Set paths for GWAS sum stats + .genes.out file (with the z-scores)
gwas_sumstats_path = gwas_sumstats_path_Intelligence # "/Users/natske/GWAS_Summary_Statistics/20016.assoc.tsv"
memoryGenesOut = sprintf("%s.genes.out",get.magma.paths(gwas_sumstats_path_Memory,upstream_kb = 10,downstream_kb = 1.5)$filePathPrefix)
ctAssocsLinear = calculate_celltype_associations(ctd,gwas_sumstats_path,genome_ref_path=genome_ref_path,specificity_species = "mouse",genesOutCOND=memoryGenesOut,
                                                 analysis_name = "ControllingForPropMemory")
saveRDS(ctAssocsLinear, file.path(storage_dir,"ctAssocsLinear_ControllingForPropMemory.rds"))
# FigsLinear = plot_celltype_associations(ctAssocsLinear,ctd=ctd,fileTag = "ControllingForPropMemory")


# We find that after controlling for prospective memory, there is no significant enrichment left associated with fluid intelligence.

## Calculate cell type enrichments directly (using linear model)

magmaGenesOut = adjust.zstat.in.genesOut(ctd = ctd,
                                         magma_GenesOut_file=file.path(storage_dir,"GWAS/MAGMA_Files",
                                                              "20016_irnt.gwas.imputed_v3.both_sexes.formatted.tsv.10UP.1.5DOWN",
                                                              "20016_irnt.gwas.imputed_v3.both_sexes.formatted.tsv.10UP.1.5DOWN.genes.out"),
                                         sctSpecies="mouse")
saveRDS(magmaGenesOut, file.path(storage_dir,"magmaGenesOut.rds"))
annotLevel <- 2
output = calculate.celltype.enrichment.probabilities.wtLimma(magmaAdjZ=magmaGenesOut,ctd = ctd,thresh=0.0001,sctSpecies="mouse")
saveRDS(output, file.path(storage_dir,"calculate.celltype.enrichment.probabilities.wtLimma_output.rds"))

# We can then get the probability of the celltype being enriched as follows

print(sort(output))


# The results should closely resemble those obtained using MAGMA

## Gene set enrichments

# To test whether a gene set (in HGNC or MGI format) is enriched using MAGMA the following commands can be used:
data("rbfox_binding")
geneset <- rbfox_binding
gwas_sumstats_path = gwas_sumstats_path_Intelligence#"/Users/natske/GWAS_Summary_Statistics/20016.assoc.tsv"
geneset_res = calculate_geneset_enrichment(geneset=geneset,
                                           gwas_sumstats_path=gwas_sumstats_path,
                                           analysis_name="Rbfox_20016",
                                           upstream_kb=10,downstream_kb=1.5,
                                           genome_ref_path=genome_ref_path,
                                           geneset_species="mouse")
saveRDS(geneset_res, file.path(storage_dir,"geneset_res.rds"))
print(geneset_res)


# We can then test whether the geneset is still enriched after controlling for celltype enrichment:
# data(ctd_allKI)
# ctd <- ctd_allKI
# ctd = prepare.quantile.groups(ctd,specificity_species="mouse",numberOfBins=40)
# analysis_name="Rbfox_16_pyrSS"
# controlledCTs = c("pyramidal SS")
# cond_geneset_res_pyrSS = calculate_conditional_geneset_enrichment(geneset = geneset,
#                                                                   ctd = ctd,
#                                                                   controlledAnnotLevel=1,
#                                                                   controlledCTs = controlledCTs,
#                                                                   gwas_sumstats_path = gwas_sumstats_path,
#                                                                   analysis_name=analysis_name,
#                                                                   genome_ref_path=genome_ref_path,
#                                                                   specificity_species = "mouse")
# controlledCTs = c("pyramidal CA1")
# cond_geneset_res_pyrCA1 = calculate_conditional_geneset_enrichment(geneset,ctd,controlledAnnotLevel=1,controlledCTs,gwas_sumstats_path,analysis_name=analysis_name,genome_ref_path=genome_ref_path,specificity_species = "mouse")
# controlledCTs = c("pyramidal CA1","pyramidal SS")
# cond_geneset_res_pyr = calculate_conditional_geneset_enrichment(geneset,ctd,controlledAnnotLevel=1,controlledCTs,gwas_sumstats_path,analysis_name=analysis_name,genome_ref_path=genome_ref_path,specificity_species = "mouse")
# controlledCTs = c("Medium Spiny Neuron")
# cond_geneset_res_MSN = calculate_conditional_geneset_enrichment(geneset,ctd,controlledAnnotLevel=1,controlledCTs,gwas_sumstats_path,analysis_name=analysis_name,genome_ref_path=genome_ref_path,specificity_species = "mouse")
# controlledCTs = c("Medium Spiny Neuron","pyramidal CA1","pyramidal SS","interneurons")
# cond_geneset_res = calculate_conditional_geneset_enrichment(geneset,ctd,controlledAnnotLevel=1,controlledCTs,gwas_sumstats_path,analysis_name=analysis_name,genome_ref_path=genome_ref_path,specificity_species = "mouse")


##### 3. Compress results folder and upload to GitHub ####
zipfile <- paste0(storage_dir,".zip")
zip(zipfile = zipfile, 
    files = storage_dir)
if(!require(piggyback)) install.packages("piggyback")
piggyback::pb_upload(file = zipfile, 
                     repo = "neurogenomics/MAGMA_Celltyping", 
                     overwrite = TRUE)
 
