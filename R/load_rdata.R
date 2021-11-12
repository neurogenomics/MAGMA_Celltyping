load_rdata <- function(fileName) {
    load(fileName)
    get(ls()[ls() != "fileName"])
}
