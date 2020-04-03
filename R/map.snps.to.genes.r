#' Map SNPs to their nearby genes
#'
#' Make two external calls to MAGMA. First use it to annotate SNPs onto their neighbouring genes. Second, use it to calculate the gene level trait association.
#'
#' @param path Filepath of the summary statistics file
#' @param upstream_kb How many kb upstream of the gene should SNPs be included?
#' @param downstream_kb How many kb downstream of the gene should SNPs be included?
#' @param N What is the N number for this GWAS? That is cases+controls
#' @param genome_ref_path Path to the folder containing the 1000 genomes .bed files (which can be downloaded from https://ctg.cncr.nl/software/MAGMA/ref_data/g1000_eur.zip)
#'
#' @return Filepath for the genes.out file
#'
#' @examples
#' genesOutPath = map.snps.to.genes(path_formatted)
#'
#' @export
map.snps.to.genes <- function(path_formatted,upstream_kb=10,downstream_kb=1.5,N=NULL,genome_ref_path){
    path_formatted = path.expand(path_formatted)
    magmaPaths = get.magma.paths(path_formatted,upstream_kb,downstream_kb)
    
    # Check whether there is an N column in the sumstats file (if it wasn't provided as an argument)
    if(is.null(N)){
        con <- file(path_formatted,"r") ; first_line <- readLines(con,n=1) ; close(con)
        column_headers = strsplit(first_line,"\t")[[1]]
        if("N" %in% column_headers){n_arg = "ncol=N"}else{
            nval <- as.numeric(readline("There is no N column within the sumstats file. What is the N value for this GWAS?"))
            
            if(is.na(nval)){stop(sprintf("%s provided but value of N for the GWAS must be numeric",nval))}
            if(nval<1000){stop("Value of N provided is less than 1000. This seems unlikely.")}
            if(nval>100000000){stop("Value of N provided is over than 100000000. In 2018 this seems unlikely.")}
            n_arg = sprintf("N=%s",nval)
        }
    }else{
        n_arg = sprintf("N=%s",N)
    }
    
    # Determine which genome build it uses & get path to gene loc file
    genome_build = get_genomebuild_for_sumstats(path_formatted)
    gene_loc_dir = sprintf("%s/data/",system.file(package="MAGMA.Celltyping"))
    if(genome_build == "GRCh37"){genomeLocFile=sprintf("%s/NCBI37.3.gene.loc",gene_loc_dir)}
    if(genome_build == "GRCh38"){genomeLocFile=sprintf("%s/NCBI38.gene.loc",gene_loc_dir)}
    print(sprintf("GWAS Sumstats appear to come from genome build: %s",genome_build))
    
    #sumstatsPrefix = sprintf("%s.%sUP.%sDOWN",path_formatted,upstream_kb,downstream_kb)
    #sumstatsPrefix = 
    outPath = gsub("\\/$","",magmaPaths$filePathPrefix) # Remove a trailing slash to avoid errors on windows
    magma_cmd = sprintf("magma --annotate window=%s,%s --snp-loc '%s' --gene-loc '%s' --out '%s'",upstream_kb,downstream_kb,path_formatted,genomeLocFile,outPath)
    system(magma_cmd)
    
    # SCHIZ CLOZUK N=35802
    magma_cmd = sprintf("magma --bfile '%s' --pval '%s' %s --gene-annot '%s.genes.annot' --out '%s'",path.expand(genome_ref_path),path_formatted,n_arg,magmaPaths$filePathPrefix,outPath)
    #magma_cmd
    system(magma_cmd)
    
    # Return path to genes.out file
    return(sprintf("%s.genes.out",path_formatted))
}