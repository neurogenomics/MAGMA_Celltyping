as_package_version <- function(version){
    
    if(length(version)>1) {
        return(base::Map(f=as_package_version, version))
    }
    version <- gsub("^v","",version, ignore.case = TRUE)
    subversion <- gsub("[^a-zA-Z]", "", version)
    if(nchar(subversion)>0){
        version <- gsub(subversion, 
                        paste0(".",match(subversion, letters)), 
                        version)    
    }
    version <- package_version(version)
    return(version)
}