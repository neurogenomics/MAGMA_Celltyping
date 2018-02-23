#' Map SNPs to their nearby genes
#'
#' Make two external calls to MAGMA. First use it to annotate SNPs onto their neighbouring genes. Second, use it to calculate the gene level trait association.
#'
#' @param path Filepath of the summary statistics file
#' @param upstream_kb How many kb upstream of the gene should SNPs be included?
#' @param downstream_kb How many kb downstream of the gene should SNPs be included?
#' @param N What is the N number for this GWAS? That is cases+controls
#'
#' @return Filepath for the genes.out file
#'
#' @examples
#' genesOutPath = map.snps.to.genes(gwas_sumstats_path)
#'
#' @export
map.snps.to.genes <- function(gwas_sumstats_path,upstream_kb=10,downstream_kb=1.5,N=NULL){
    # Check whether there is an N column in the sumstats file (if it wasn't provided as an argument)
    if(is.null(N)){
        con <- file(path,"r") ; first_line <- readLines(con,n=1) ; close(con)
        column_headers = strsplit(first_line,"\t")[[1]]
        if("N" %in% column_headers){n_arg = "ncol=N"}else{
            nval <- readline("There is no N column within the sumstats file. What is the N value for this GWAS?")
            if(!is.numeric(nval)){stop("Value of N for the GWAS must be numeric")}
            iff(nval<1000){stop("Value of N provided is less than 1000. This seems unlikely.")}
            iff(nval>100000000){stop("Value of N provided is over than 100000000. In 2018 this seems unlikely.")}
            n_arg = sprintf("N=%s",nval)
        }
    }
    
    # Determine which genome build it uses & get path to gene loc file
    genome_build = get_genomebuild_for_sumstats(gwas_sumstats_path)
    gene_loc_dir = sprintf("%s/data/",system.file(package="MAGMA.Celltyping"))
    if(genome_build == "GRCh37"){genomeLocFile=sprintf("%s/NCBI37.3.gene.loc",gene_loc_dir)}
    if(genome_build == "GRCh38"){genomeLocFile=sprintf("%s/NCBI38.gene.loc",gene_loc_dir)}
    print(sprintf("GWAS Sumstats appear to come from genome build: %s",genome_build))
    
    magma_cmd = sprintf("%smagma --annotate window=%s,%s --snp-loc '%s' --gene-loc '%s' --out '%s'",magma_path,kb_upstream,kb_downstream,gwas_sumstats_path,genomeLocFile,gwas_sumstats_path)
    system(magma_cmd)
    
    # SCHIZ CLOZUK N=35802
    magma_cmd = sprintf("%smagma --bfile '%s' --pval '%s' %s --gene-annot '%s.genes.annot' --out '%s'",magma_path,genome_ref_dir,gwas_sumstats_path,n_arg,gwas_sumstats_path,gwas_sumstats_path)
    magma_cmd
    system(magma_cmd)
    
    # Return path to genes.out file
    return(sprintf("%s.genes.out",gwas_sumstats_path))
}