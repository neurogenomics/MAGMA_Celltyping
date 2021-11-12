#' Downloads and loads the SNP_LOC_DATA file that I stashed at figshare
#'
#' @return SNP_LOC_DATA Has four columns with SNP CHR BP and Build
#'
#' @examples
#' SNP_LOC_DATA <- load_snp_loc_data()
#' @export
#' @importFrom utils download.file
load_snp_loc_data <- function() {
    # print("There is no SNP column found within the data. It must be inferred from CHR and BP information.")
    # print("Note: this requires downloading a 300mb file from figshare into a temporary directory")
    # print("the file which is downloaded is created by the build_snp_location_tables function included with this package")
    SNP_LOC_DATA <- NA # Because SNP_LOC_DATA is loaded from a file, we need to trick devtools::check() into passing with this
    filePath <- sprintf("%s/MAGMA.Celltyping/data/SNP_LOC_DATA.rda", .libPaths()[1])
    if (!file.exists(filePath)) {
        message("Downloading 300MB+ SNP_LOC_DATA file ==> ", filePath)
        start.time <- Sys.time()
        utils::download.file("https://ndownloader.figshare.com/files/21768105",
            destfile = filePath
        )
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        print(sprintf("File downloaded in %.0f seconds", time.taken))
    } else {
        message("Loading pre-downloaded SNP_LOC_DATA file: ", filePath)
    }
    load(filePath)
    ### benchmark
    # qs_path <- gsub(".rda",".qs",filePath)
    # qs::qsave(SNP_LOC_DATA, qs_path)
    # microbenchmark::microbenchmark(load(filePath),
    #                                SNP_LOC_DATA <- qs::qload(qs_path))
    # data.table::fwrite(SNP_LOC_DATA,"~/Desktop/SNP_LOC_DATA.tsv.gz", sep="\t")
    # saveRDS(SNP_LOC_DATA, "~/Desktop/SNP_LOC_DATA.rds")
    # microbenchmark::microbenchmark(load(filePath),
    #                                SNP_LOC_DATA2 <- data.table::fread("~/Desktop/SNP_LOC_DATA.tsv.gz"))
    return(SNP_LOC_DATA)
}
