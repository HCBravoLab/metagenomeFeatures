#' MgDb-class object with Greengenes Database Version 13.8 97\% OTUs
#'
#' Example \link[=MgDb]{MgDb-class} object with Greengenes Database Version
#' 13.8 97\% OTUs.
#' @return MgDb-class object
#' @export
#' @examples
#' get_gg13.8_97MgDb()
get_gg13.8_85MgDb <- function(){
    ## Source data generated using the make_greengenes13.8_85.R script in inst/scripts

    metadata_file <- system.file("extdata", 'gg13.8_85_metadata.RData',
                           package = "metagenomeFeatures")
    load(metadata_file)

    gg_db_file <- system.file("extdata", 'gg13.8_85.sqlite',
                                package = "metagenomeFeatures")

    gg_tree_file <- system.file("extdata", "gg13.8_85.tre",
                                  package = "metagenomeFeatures")

    ## Creating a new MgDb class object with gg13.8_85 data
    newMgDb(db_file = gg_db_file,
            tree = gg_tree_file,
            metadata =  metadata)
}
