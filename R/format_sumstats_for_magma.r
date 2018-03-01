#' Check that sumstats has correct columns and that they are in the correct order for MAGMA and LDSC
#'
#' @return col_headers The new column headers for the sumstats file
#'
#' @examples
#' format_sumstats_for_magma(path)
#'
#' @import data.table
#' @import stringr
#' @export
format_sumstats_for_magma <- function(path){
    # Check the sumstats file exists
    if(!file.exists(path)){stop("Path to GWAS sumstats is not valid")}
    
    col_headers = standardise.sumstats.column.headers(path)
    
    
    # Check if there are CHR and BP columns
    if(!sum(c("SNP","BP") %in% col_headers)==2){
        # If not, see if there is a column storing the data in a format like: CHR:BP:A2:A1 or just CHR:BP
        # - UKBB data from Ben Neale has a Variant column with CHR:POS:REF:ALT where ALT allele is the effect allele in the model [NB: the ALT allele is NOT always the minor allele]
        # -- For input to LDSC, A1 is effect allele, A2 is non-effect allele
        # - DIAGRAM diabetes data has a Chr:Position column with CHR:BP
        # - BMI adjusted for smoking has markername with CHR:BP (with the chromosome name having 'chr' preceeding)
        # - Agression [EAGLE] just doesn't have any CHR or BP data
        print("Summary statistics file does not have obvious CHR or BP columns. Checking to see if they are joined in another column")
        
        # Obtain a row of the actual data
        con <- file(path,"r") ; row_of_data <- strsplit(readLines(con,n=2)[2],"\t")[[1]] ; close(con)
        
        # Check if there is a column of data with CHR:BP:A2:A1 format
        fourStepCol = grep(".*:.*:\\w:\\w",row_of_data)
        if(length(fourStepCol)){
            # Convert the ':' into '\t'
            awkSplitCmd = sprintf("gawk -i inplace -F\":\" '$1=$1' OFS=\"\t\" %s",path,path)
            system2("/bin/bash", args = c("-c", shQuote(awkSplitCmd)))
            # Replace the column name with four names
            curColName = col_headers[fourStepCol]
            # Write the new column headers to file
            first_line = paste(col_headers,collapse = "\t")
            new_first_line = gsub(curColName,"CHR\tBP\tA2\tA1",paste(col_headers,collapse = "\t"))
            sed_command = sprintf("sed -i '' '1s/%s/%s/' %s",first_line,new_first_line,path)
            system2("/bin/bash", args = c("-c", shQuote(sed_command)))
            col_headers = strsplit(new_first_line,"\t")[[1]]
            print(sprintf("Column %s has been replaced with CHR BP A2 A1",curColName))
            print(col_headers)
            con <- file(path,"r") ; row_of_data <- strsplit(readLines(con,n=2)[2],"\t")[[1]] ; close(con)
        }
        
        # Check if there is a column of data with CHR:BP format
        twoStepCol = grep(".*:.*",row_of_data)
        if(length(twoStepCol)){
            # Convert the ':' into '\t'
            awkSplitCmd = sprintf("gawk -i inplace -F\":\" '$1=$1' OFS=\"\t\" %s",path,path)
            system2("/bin/bash", args = c("-c", shQuote(awkSplitCmd)))
            # Replace the column name with four names
            curColName = col_headers[twoStepCol]
            # Write the new column headers to file
            first_line = paste(col_headers,collapse = "\t")
            new_first_line = gsub(curColName,"CHR\tBP",paste(col_headers,collapse = "\t"))
            sed_command = sprintf("sed -i '' '1s/%s/%s/' %s",first_line,new_first_line,path)
            system2("/bin/bash", args = c("-c", shQuote(sed_command)))
            col_headers = strsplit(new_first_line,"\t")[[1]]
            print(sprintf("Column %s has been replaced with CHR BP A2 A1",curColName))
            print(sprintf("Column %s has been replaced with CHR BP",curColName))
            print(col_headers)
            con <- file(path,"r") ; row_of_data <- strsplit(readLines(con,n=2)[2],"\t")[[1]] ; close(con)
        }
    }
    
    # Check that all the vital columns are present
    con <- file(path,"r") ; rows_of_data <- readLines(con,n=2) ; close(con)
    for(key_column in c("SNP","CHR","BP","P","A1","A2")){
        code_example = "sed -i '' '1s/p_value/P/' IQ.Sniekers.2017.txt"
        if(!key_column %in% col_headers){
            print("Header of file:")
            #system(sprintf("head -n 3 %s",path))
            print(rows_of_data)
            stop(sprintf("cannot find a %s column in GWAS sumstats file. \nUse code such as '%s' to fix",key_column,code_example))
        }
    }
    
    # Check there is at least one signed sumstats column
    signed_stat_column_names = c("Z","OR","BETA","LOG_ODDS","SIGNED_SUMSTAT")
    if(sum(signed_stat_column_names %in% col_headers)<1 %in% col_headers){
        print("Header of file:")
        print(rows_of_data)
        stop("ERROR: cannot find a column name representing signed statistic in GWAS sumstats file. I.e. Z, OR, BETA")
    }
    
    # Check that first three column headers are SNP, CHR, BP (in that order)
    if(!sum(col_headers[1:3]==c("SNP","CHR","BP"))==3){
        whichSNP = which(col_headers=="SNP")
        whichCHR = which(col_headers=="CHR")
        whichBP = which(col_headers=="BP")
        otherCols = setdiff(1:length(col_headers),c(whichSNP,whichCHR,whichBP))
        #system(sprintf("gawk -i inplace '{ print $%s \" \" $%s \" \" $%s}' %s",whichSNP,whichCHR,whichBP,path))
        #newColOrder = sprintf("$%s",paste(c(whichSNP,whichCHR,whichBP,otherCols),collapse = " \" \" $"))
        newColOrder = sprintf("$%s",paste(c(whichSNP,whichCHR,whichBP,otherCols),collapse = " \"\\t\" $"))
        system(sprintf("gawk -i inplace '{ print %s}' %s",newColOrder,path))
    }
    
    # MAGMA cannot handle P-values as low as 3e-400... so convert them to zeros
    shCmd = sprintf("gawk -i inplace '{ i=%s; if($i > 1) { $i=0; }  print }' %s",which(col_headers=="P"),path.expand(path))
    system2("/bin/bash", args = c("-c", shQuote(shCmd)))
    
    # Show how the data now looks
    print("Succesfully finished preparing sumstats file:")
    print("Header of file:")
    con <- file(path,"r") ; rows_of_data <- readLines(con,n=2) ; close(con)
    print(rows_of_data)
    col_headers = strsplit(rows_of_data[1],"\t")[[1]]
    
    # Drop any rows with duplicated RSIDs
    shCmd = sprintf("gawk -i inplace '!seen[$1]++' '%s'",path.expand(path))
    system2("/bin/bash", args = c("-c", shQuote(shCmd)))
    
    return(col_headers)
}


#all_sumstats <- readLines(path,n=-1)
#first_line = all_sumstats[1]
