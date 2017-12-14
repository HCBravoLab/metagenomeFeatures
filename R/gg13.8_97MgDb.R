#' MgDb-class object with Greengenes Database Version 13.8 97% OTUs
#'
#' Example \link[=MgDb]{MgDb-class} object with Greengenes Database Version
#' 13.8 97% OTUs.
#' @return MgDb-class object
#' @export
#' @examples
#'  get_gg13.8_97MgDb()
get_gg13.8_97MgDb <- function(){
    ## Source data generated using the make_greengenes13.8_97.R script in inst/scripts

    metadata_file <- system.file("extdata", 'gg13.8_97_metadata.RData',
                           package = "metagenomeFeatures")

    gg_db_file <- system.file("extdata", 'gg13.8_97.sqlite',
                                package = "metagenomeFeatures")

    gg_tree_file <- system.file("extdata", "gg13.8_97.tre",
                                  package = "metagenomeFeatures")

    ## Creating a new MgDb class object with gg13.8_97 data
    newMgDb(db_file = gg_db_file,
            tree = gg_tree_file,
            metadata =  load(metadata_file))
}
