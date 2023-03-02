check_analysis_name <- function(EnrichmentMode,
                                analysis_name){
    EnrichmentMode_nm <- gsub("%","pct",gsub("[ ]","",EnrichmentMode))
    #### Only add EnrichmentMode_nm if it hasn't been added already ####
    if(!grepl(EnrichmentMode_nm,analysis_name)){
        analysis_name <- paste0(analysis_name,".",EnrichmentMode_nm)
    } 
    return(analysis_name)
}