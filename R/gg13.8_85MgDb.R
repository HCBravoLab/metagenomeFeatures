#' MgDb-class object with Greengenes Database Version 13.8 85\% OTUs
#'
#' Example \link[=MgDb-class]{MgDb-class} object with Greengenes Database Version
#' 13.8 85\% OTUs.
#' @return MgDb-class object
#' @export
#' @examples
#' get_gg13.8_85MgDb()
get_gg13.8_85MgDb <- function(){
    ## Data generated using make_greengenes13.8_85.R script in inst/scripts
    metadata <- list(ACCESSION_DATE = date(),
                     URL = "ftp://greengenes.microbio.me/greengenes_release/gg_13_8_otus",
                     DB_TYPE_NAME = "GreenGenes",
                     DB_VERSION = "13.8 85% OTUS",
                     DB_TYPE_VALUE = "MgDb",
                     DB_SCHEMA_VERSION = "2.0")

    gg_db_file <- system.file("extdata", 'gg13.8_85.sqlite',
                                package = "metagenomeFeatures")

    gg_tree_file <- system.file("extdata", "gg13.8_85.tre",
                                  package = "metagenomeFeatures")

    ## Creating a new MgDb class object with gg13.8_85 data
    newMgDb(db_file = gg_db_file,
            tree = gg_tree_file,
            metadata =  metadata)
}
