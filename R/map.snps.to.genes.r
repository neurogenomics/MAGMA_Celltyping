#' Map SNPs to their nearby genes
#'
#' Make two external calls to MAGMA. First use it to annotate SNPs onto their neighbouring genes. Second, use it to calculate the gene level trait association.
#'
#' @param path_formatted Filepath of the summary statistics file
#' (which is expected to already be in the required format).
#' Can be uncompressed or compressed (".gz" or ".bgz").
#' @param genome_build The build of the reference genome (\code{"GRCh37"} or \code{"GRCh38"}).
#' If \code{NULL}, it will be inferred with \link[MungeSumstats]{get_genome_build}.
#' @param upstream_kb How many kb upstream of the gene should SNPs be included?
#' @param downstream_kb How many kb downstream of the gene should SNPs be included?
#' @param N What is the N number for this GWAS? That is cases+controls
#' @param genome_ref_path Path to the folder containing the 1000 genomes .bed files
#'  (which can be downloaded from https://ctg.cncr.nl/software/MAGMA/ref_data/g1000_eur.zip)
#' @param force_new Set to \code{TRUE} to rerun \code{MAGMA} even if the output files already exist.
#'  \emph{DEFAULT=}\code{FALSE}.
#'
#' @return Filepath for the genes.out file
#'
#' @examples
#' # genesOutPath = map.snps.to.genes(path_formatted)
#'
#' @export
map.snps.to.genes <- function(path_formatted,
                              genome_build=NULL,
                              upstream_kb=10,
                              downstream_kb=1.5,
                              N=NULL,
                              genome_ref_path,
                              force_new=FALSE){

    path_formatted = path.expand(path_formatted)
    magmaPaths = get.magma.paths(path_formatted,upstream_kb,downstream_kb)
    # Remove a trailing slash to avoid errors on windows
    outPath = gsub("\\/$","",magmaPaths$filePathPrefix)
    genes_annot <- sprintf("%s.genes.annot",outPath)
    genes_out <- sprintf("%s.genes.out",outPath)

    if((file.exists(genes_annot) & file.exists(genes_out)) &
       (force_new==FALSE)){
        message("Precomputed file detected: ",genes_out)
        return(genes_out)
    }

    #### MAGMA requires files to be decompressed #####
    path_formatted <- decompress(path_formatted = path_formatted)

    # Check whether there is an N column in the sumstats file (if it wasn't provided as an argument)
    if(is.null(N)){
        #first_line <- MungeSumstats:::read_header(path = path_formatted, n = 1)
        first_line <-
            as.character(MungeSumstats:::read_header(path = path_formatted,
                                                        n = 1))
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
    gene_loc_dir = sprintf("%s/data/",system.file(package="MAGMA.Celltyping"))
    if(is.null(genome_build)){
        genome_build <- MungeSumstats::get_genome_build(sumstats = path_formatted)
    }
    if(toupper(genome_build) == "GRCH37"){genomeLocFile=sprintf("%s/NCBI37.3.gene.loc",gene_loc_dir)}
    if(toupper(genome_build) == "GRCH38"){genomeLocFile=sprintf("%s/NCBI38.gene.loc",gene_loc_dir)}
    print(sprintf("GWAS Sumstats appear to come from genome build: %s",genome_build))

    #sumstatsPrefix = sprintf("%s.%sUP.%sDOWN",path_formatted,upstream_kb,downstream_kb)

    #### Create genes.annot ####
    magma_cmd = sprintf("magma --annotate window=%s,%s --snp-loc '%s' --gene-loc '%s' --out '%s'",
                        upstream_kb,downstream_kb,path_formatted,genomeLocFile,outPath)
    system(magma_cmd)

    #### Create genes.out ####
    magma_cmd = sprintf("magma --bfile '%s' --pval '%s' %s --gene-annot '%s.genes.annot' --out '%s'",
                        path.expand(genome_ref_path),path_formatted,n_arg,magmaPaths$filePathPrefix,outPath)
    #magma_cmd
    system(magma_cmd)

    # Return path to genes.out file
    return(genes_out)
}
