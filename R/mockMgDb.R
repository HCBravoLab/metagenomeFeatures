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

# grab filenames 

mock_seq <- system.file("extdata", "mockSeq.rds", package = "metagenomeFeatures")


db_seq <- readRDS(mock_seq)

mock_taxa_file <- system.file("extdata", "mockTaxa.sqlite", package = "metagenomeFeatures")

mock_tree_file <- system.file("extdata", "msd16S_MgDb_tree.rds", package="metagenomeFeatures")


# leave tree_data empty for now--greengene13.5 database does not have a reference tree but the 13.8 versions do
# tree_data <- mgDb_select(mgdb, type = "tree", keys = genus_keys, keytype = "Keys")

mock_MgDb <- new("MgDb",
        seq = db_seq,
        taxa_file = mock_taxa_file,
        tree_file = mock_tree_file,
        metadata = metadata)
}
