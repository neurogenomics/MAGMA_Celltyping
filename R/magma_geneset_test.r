# geneSetName = "Parkinson_pdGenesAndInteractors"
# geneSet = geneList
#
# data(all_hgnc_wtEntrez)
# entrezSet = all_hgnc_wtEntrez[all_hgnc_wtEntrez$hgnc_symbol %in% geneSet,]$entrezgene
# fileEntry = paste(c(geneSetName,entrezSet),collapse=" ")
# geneCovarFile=tempfile()
# gwas_sumstats_path = path.expand(gwas_sumstats_path)
# write.table(fileEntry,file=geneCovarFile,quote=FALSE,row.names=FALSE,sep="\t",col.names=FALSE)
#
# upstream_kb = 10
# downstream_kb=1.5
# sumstatsPrefix = sprintf("%s.%sUP.%sDOWN",gwas_sumstats_path,upstream_kb,downstream_kb)
# sumstatsPrefix2 = sprintf("%s.GENESET.%s.%sUP.%sDOWN",gwas_sumstats_path,geneSetName,upstream_kb,downstream_kb)
# magma_cmd = sprintf("magma --gene-results '%s.genes.raw' --set-annot '%s' --out '%s.%s'",sumstatsPrefix,geneCovarFile,sumstatsPrefix2,geneSetName)
#
# print(magma_cmd)
# system(magma_cmd)
#
#
# genesOut = fread("/Users/natske/GWAS_Summary_Statistics/IQ_Posthuma_2018/IQ_update_total.10UP.1.5DOWN.genes.out")
