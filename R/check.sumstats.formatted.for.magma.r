check.sumstats.formatted.for.magma <- function(path){
    if(!file.exists(path)){stop("ERROR: path to GWAS sumstats is not valid")}
    all_sumstats <- readLines(path,n=-1)
    first_line = all_sumstats[1]
    print("First line: ")
    print(first_line)
    column_headers = strsplit(first_line,"\t")[[1]]
    #print(column_headers)
    data(sumstatsColHeaders)
    for(headerI in 1:dim(sumstatsColHeaders)[1]){
        un = sumstatsColHeaders[headerI,1]
        cr = sumstatsColHeaders[headerI,2]
        #print(un)             
        if(un %in% column_headers){column_headers=gsub(sprintf("^%s$",un),cr,column_headers)}       
        if(tolower(un) %in% column_headers){column_headers=gsub(sprintf("^%s$",tolower(un)),cr,column_headers)}               
    }
    column_headers = toupper(column_headers)
    all_sumstats[1] = paste(column_headers,collapse = "\t")
    writeLines(all_sumstats,path)
    
    # Check that all the key columns are there
    for(key_column in c("SNP","CHR","BP","P","A1","A2")){
        code_example = "sed -i '' '1s/p_value/P/' IQ.Sniekers.2017.txt"
        if(!key_column %in% column_headers){
            print("Header of file:")
            system(sprintf("head -n 3 '%s'",path))
            stop(sprintf("cannot find a %s column in GWAS sumstats file. \nUse code such as '%s' to fix",key_column,code_example))
        }
    }
    signed_stat_column_names = c("Z","OR","BETA","LOG_ODDS","SIGNED_SUMSTAT")
    if(sum(signed_stat_column_names %in% column_headers)<1 %in% column_headers){
        print("Header of file:")
        system(sprintf("head -n 3 '%s'",path))
        stop("ERROR: cannot find a column name representing signed statistic in GWAS sumstats file. I.e. Z, OR, BETA")
    }
    
    # Check that first three column headers are SNP, CHR, BP (in that order)
    if(!sum(column_headers[1:3]==c("SNP","CHR","BP"))==3){
        whichSNP = which(column_headers=="SNP")
        whichCHR = which(column_headers=="CHR")
        whichBP = which(column_headers=="BP")
        system(sprintf("awk '{ print $%s \" \" $%s \" \" $%s}' %s > %s",whichSNP,whichCHR,whichBP,path,path))
    }
    print("Header of file:")
    system(sprintf("head -n 3 '%s'",path))
    return(column_headers)
}