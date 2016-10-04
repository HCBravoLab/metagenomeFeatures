#' Mock MgDb-class object
#'
#' Mock \link[=MgDb]{MgDb-class} object from the Greengenes 13.5 database.
#' @return MgDb-class object
#' @export
#' @examples
#' get_mockMgDb()
get_mockMgDb <- function(){



    metadata <- list(ACCESSION_DATE = "3/31/2015",
                     URL = "https://greengenes.microbio.me",
                     DB_TYPE_NAME = "GreenGenes-MgDb-Mock",
                     DB_TYPE_VALUE = "MgDb",
                     DB_SCHEMA_VERSION = "1.0",
                     NOTE = "URL does not work, working link here: http://greengenes.secondgenome.com/downloads/database/13_5")

    # get db seq data and filenames
    mock_seq <- system.file("extdata", "mock_MgDb_seq.fasta.gz",
                            package = "metagenomeFeatures") %>%
        Biostrings::readDNAStringSet()

    mock_taxa_file <- system.file("extdata", "mock_MgDb_taxa.sqlite",
                                  package = "metagenomeFeatures")

    # mock_tree_file <- system.file("extdata", "msd16s_MgDb_tree.rds",
    #                               package="metagenomeFeatures")


    #  tree data slot is empty - not reference tree for gg 13.5
    # tree_data <- mgDb_select(mgdb, type = "tree", keys = genus_keys, keytype = "Keys")

    new("MgDb",
        seq = mock_seq,
        taxa_file = mock_taxa_file,
        tree_file = "not available", #mock_tree_file,
        metadata = metadata)
}
