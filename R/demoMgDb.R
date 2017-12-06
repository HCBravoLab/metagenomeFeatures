#' Example MgDb-class object
#'
#' Example \link[=MgDb]{MgDb-class} object with 3211 entries from the Greengenes
#' 13.8 OTU 99 database.
#' @return MgDb-class object
#' @export
#' @examples
#'  get_demoMgDb()
get_demoMgDb <- function(){
    metadata <- list(ACCESSION_DATE = "3/31/2015",
                     URL = "https://greengenes.microbio.me",
                     DB_TYPE_NAME = "GreenGenes",
                     DB_TYPE_VALUE = "MgDb",
                     DB_SCHEMA_VERSION = "1.0")

    demo_db_file <- system.file("extdata", 'mockDB.sqlite',
                                 package = "metagenomeFeatures")

    demo_tree_file <- system.file("extdata", "mockTree.rds",
                                  package = "metagenomeFeatures")

    ## Creating a new MgDb class object with demoMgDb data
    newMgDb(db_file = demo_db_file, tree = demo_tree_file, metadata)
}
