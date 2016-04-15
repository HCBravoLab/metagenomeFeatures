#' Example MgDb-class object
#'
#' Example \link[=MgDb]{MgDb-class} object with 249 entries from the Greengenes 13.5 database.
#' @return MgDb-class object
#' @export
#' @examples
#'  get_demoMgDb()
get_demoMgDb <- function(){
    metadata <- list(ACCESSION_DATE = "3/31/2015",
                     URL = "https://greengenes.microbio.me",
                     DB_TYPE_NAME = "GreenGenes-MgDb-Demo",
                     DB_TYPE_VALUE = "MgDb",
                     DB_SCHEMA_VERSION = "1.0")

    demo_seq_file <- system.file("extdata", 'demoSeq.fasta.gz',
                package="metagenomeFeatures")
    db_seq <- Biostrings::readDNAStringSet(demo_seq_file)
    #demo_seq_file)

    demo_taxa_file <- system.file("extdata", "demoTaxa.sqlite",
                                  package="metagenomeFeatures")

    demo_tree_file <- system.file("extdata", "msd16S_MgDb_tree.rds",
                                  package="metagenomeFeatures")
    ## not sure how to assign to environment instead of returning value, rda not appropriate as the connection to sqlite would expire
    ## How to find file path? maybe just create
    demoMgDb <- new("MgDb",
        seq = db_seq,
        taxa_file = demo_taxa_file,
        tree_file = demo_tree_file,
        metadata = metadata)
}
