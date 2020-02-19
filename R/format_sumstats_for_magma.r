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
    
    ## NOTE, IF YOU GET AN ERROR ABOUT NOT BEING ABLE TO FIND COLUMN, THEN MAYBE SPACES NEED EXPANDING TO TABS?
    # awk -v OFS="\t" '$1=$1' /Users/natske/GWAS_Summary_Statistics/Diabetes_Type2_Xue_et_al_T2D_META_Nat_Commun_2018.txt > /Users/natske/GWAS_Summary_Statistics/Diabetes_Type2_Xue_et_al_T2D_META_Nat_Commun_2018.expanded.txt
    # awk -v OFS="\t" '$1=$1' /Users/natske/GWAS_Summary_Statistics/MAGIC_FastingGlucose.txt > /Users/natske/GWAS_Summary_Statistics/MAGIC_FastingGlucose.expanded.txt
    # awk -v OFS="\t" '$1=$1' /Users/natske/GWAS_Summary_Statistics/MAGIC_ln_FastingInsulin.txt > /Users/natske/GWAS_Summary_Statistics/MAGIC_ln_FastingInsulin.expanded.txt
    
    # Ensure that tabs seperate rows
    con <- file(path,"r") ; row_of_data <- strsplit(readLines(con,n=2)[2],"\t")[[1]] ; close(con)
    tmpPath = tempfile()
    if(length(row_of_data)==1){
        if(grep(" ",row_of_data)==1){
            cmd = sprintf("awk -v OFS=\"\t\" '$1=$1' %s > %s",path,tmpPath)
            system(cmd)
            cmd = sprintf("mv %s %s",tmpPath,path)
            system(cmd)
        }
    }
    
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
            print(sprintf("Column %s has been replaced with CHR BP",curColName))
            print(col_headers)
            con <- file(path,"r") ; row_of_data <- strsplit(readLines(con,n=2)[2],"\t")[[1]] ; close(con)
        }
        
        # Restandardise incase the joined column headers were unusual
        col_headers = standardise.sumstats.column.headers(path)
    }
    
    # If SNP is present... BUT not CHR or BP then need to find the relevant locations
    con <- file(path,"r") ; rows_of_data <- readLines(con,n=2) ; close(con); col_headers = strsplit(rows_of_data[1],"\t")[[1]]
    if(sum(c("CHR","BP") %in% col_headers)==0 & sum("SNP" %in% col_headers)==1){
        #library(data.table)
        #sumstats = fread(path)
        #data("SNP_LOC_DATA")
        #SNP_LOC_DATA_2 = SNP_LOC_DATA[,1:3]
        #sumstats2 = merge(sumstats,SNP_LOC_DATA_2,by="SNP")
        #sumstats3 = data.frame(sumstats2)[,c("SNP","CHR","BP",setdiff(colnames(sumstats2),c("SNP","CHR","BP")))]
        #fwrite(sumstats3,file=path,sep="\t")
        stop("I've blocked this function because SNP_LOC_DATA is incomplete. Try rebuilding it with data from ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606/database/organism_data/b151_SNPContigLocusId_108.bcp.gz?")
    }
    
    # If CHR and BP are present... BUT not SNP then need to find the relevant SNP ids
    con <- file(path,"r") ; rows_of_data <- readLines(con,n=2) ; close(con); col_headers = strsplit(rows_of_data[1],"\t")[[1]]
    if(sum(c("CHR","BP") %in% col_headers)==2 & sum("SNP" %in% col_headers)==0){

        print("There is no SNP column found within the data. It must be inferred from CHR and BP information.")
        print("Note: this requires downloading a 300mb file from figshare into a temporary directory")
        print("the file which is downloaded is created by the build_snp_location_tables function included with this package")
        tmpF1 = tempfile()
        #download.file("https://ndownloader.figshare.com/files/21768105",destfile=tmpF1)
        load(tmpF1)
        genomebuild <- as.numeric(readline("Which genome build is the data from? 1 for GRCh37, 2 for GRCh38"))
        if(!genomebuild %in% c(1,2)){stop("Genome build must be entered as either 1 (for GRCh37) or 2 (for GRCh38)")}
        data("SNP_LOC_DATA")
        if(genomebuild==1){genomebuild="GRCh37"}else{genomebuild="GRCh38"}
        snpLocDat = SNP_LOC_DATA[SNP_LOC_DATA$Build==genomebuild,][,-4]
        library(data.table)
        sumstats = fread(path)
        sumstats$CHR = as.factor(sumstats$CHR)
        if(length(grep("chr",sumstats$CHR[1]))!=0){sumstats$CHR = gsub("chr","",sumstats$CHR)}
        sumstats2 = merge(sumstats,snpLocDat,by=c("CHR","BP"))
        fwrite(sumstats2,file=path,sep="\t")
        #stop("I've blocked this function because SNP_LOC_DATA is incomplete.  Try rebuilding it with data from ftp://ftp.ncbi.nih.gov/snp/organisms/human_9606/database/organism_data/b151_SNPContigLocusId_108.bcp.gz?")        
    }
    
    # Check that all the vital columns are present
    con <- file(path,"r") ; rows_of_data <- readLines(con,n=2) ; close(con); col_headers = strsplit(rows_of_data[1],"\t")[[1]]
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
        whichSNP = which(col_headers=="SNP")[1]
        whichCHR = which(col_headers=="CHR")[1]
        whichBP = which(col_headers=="BP")[1]
        otherCols = setdiff(1:length(col_headers),c(whichSNP,whichCHR,whichBP))
        #system(sprintf("gawk -i inplace '{ print $%s \" \" $%s \" \" $%s}' %s",whichSNP,whichCHR,whichBP,path))
        #newColOrder = sprintf("$%s",paste(c(whichSNP,whichCHR,whichBP,otherCols),collapse = " \" \" $"))
        newColOrder = sprintf("$%s",paste(c(whichSNP,whichCHR,whichBP,otherCols),collapse = " \"\\t\" $"))
        system(sprintf("gawk -i inplace '{ print %s}' %s",newColOrder,path))
    }
    
    # The formatting process can (rarely) result in duplicated columns, i.e. CHR, if CHR:BP is expanded and one already exists... delete duplicates
    con <- file(path,"r") ; rows_of_data <- readLines(con,n=2) ; close(con); col_headers = strsplit(rows_of_data[1],"\t")[[1]]
    if(sum(duplicated(col_headers))>0){
        notDup = which(!duplicated(col_headers))
        newColOrder = sprintf("$%s",paste(notDup,collapse = " \"\\t\" $"))
        system(sprintf("gawk -i inplace '{ print %s}' %s",newColOrder,path))        
    }
    
    # MAGMA cannot handle P-values as low as 3e-400... so convert them to zeros
    con <- file(path,"r") ; rows_of_data <- readLines(con,n=2) ; close(con); col_headers = strsplit(rows_of_data[1],"\t")[[1]]
    shCmd = sprintf("gawk -i inplace '{ i=%s; if($i > 1) { $i=0; }  print }' %s",which(col_headers=="P"),path.expand(path))
    system2("/bin/bash", args = c("-c", shQuote(shCmd)))
    
    # The above command converts tabs in the header to spaces... so revert that (if neccesary)
    con <- file(path,"r") ; rows_of_data <- readLines(con,n=2) ; close(con)
    #if(("2" %in% grep("\t",rows_of_data)) & length(grep("\t",rows_of_data))==1){
    if(length(grep("\t",rows_of_data))!=2){
        tmpName = tempfile()
        shCmd = sprintf("tr ' ' '\t' < %s <> %s > %s",path.expand(path),path.expand(path),tmpName)
        system2("/bin/bash", args = c("-c", shQuote(shCmd)))
        shCmd = sprintf("mv %s %s",tmpName,path.expand(path))
        system2("/bin/bash", args = c("-c", shQuote(shCmd)))
    }
    # The above command converts 'P' to '0'... so revert that
    shCmd = sprintf("sed -i '' '1s/0/P/' '%s'",path.expand(path))
    system2("/bin/bash", args = c("-c", shQuote(shCmd)))
    
    
    # Sometimes the N column is not all integers... so round it up
    con <- file(path,"r") ; rows_of_data <- readLines(con,n=2) ; close(con); col_headers = strsplit(rows_of_data[1],"\t")[[1]]
    if("N" %in% col_headers){
        whichN = which(col_headers %in% "N")
        #shCmd = sprintf("gawk -i inplace '{OFS=FS="\t"}NR>1{$%s=sprintf("%3.0f",$%s)}1' %s",whichN,whichN,path.expand(path))
        pt1 = "gawk -i inplace '{OFS=FS=\"\t\"}NR>1{$"
        pt2 = whichN
        pt3 = "=sprintf(\"%3.0f\",$"
        pt4 = whichN
        pt5 = ")}1' "
        pt6 = path.expand(path)
        shCmd = paste(c(pt1,pt2,pt3,pt4,pt5,pt6),collapse="")
        system2("/bin/bash", args = c("-c", shQuote(shCmd)))
    }
     
    # All rows should start with either SNP or rs... if they don't drop them
    shCmd = sprintf("grep -e '^rs' -e '^SNP' '%s' > '%s_tmp'",path.expand(path),path.expand(path))
    system2("/bin/bash", args = c("-c", shQuote(shCmd)))
    shCmd = sprintf("mv %s_tmp %s",path.expand(path),path.expand(path))
    system2("/bin/bash", args = c("-c", shQuote(shCmd)))     
    
    # Keep only rows which have the number of columns expected
    shCmd = sprintf("gawk -i inplace -F'\t' 'NF == %s {print}' '%s'",length(col_headers),path.expand(path))
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
