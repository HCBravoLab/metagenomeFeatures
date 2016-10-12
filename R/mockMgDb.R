#' Mock MgDb-class object
#'
#' Mock \link[=MgDb]{MgDb-class} object with a subset of the GreenGenes 13.9 OTU 0.99
#' database including the ids for OTU from a mock community dataset from the
#' study Bokulich et al. 2013, the OTU ids were extracted from a biom file
#' downloaded from QIITA (https://qiita.ucsd.edu).
#'
#' Bokulich, Nicholas A., et al. "Quality-filtering vastly improves diversity
#' estimates from Illumina amplicon sequencing." Nature methods 10.1 (2013): 57-59.
#'
#' @return MgDb-class object
#' @export
#' @examples
#' get_mockMgDb()
get_mockMgDb <- function(){
    ## note same as demoMgDb
    metadata <- list(ACCESSION_DATE = "3/31/2015",
                     URL = "https://greengenes.microbio.me",
                     DB_TYPE_NAME = "GreenGenes",
                     DB_TYPE_VALUE = "MgDb",
                     DB_SCHEMA_VERSION = "1.0")

    demo_seq_file <- system.file("extdata", 'mockSeq.fasta.gz',
                                 package="metagenomeFeatures")
    db_seq <- Biostrings::readDNAStringSet(demo_seq_file)

    demo_taxa_file <- system.file("extdata", "mockTaxa.sqlite",
                                  package="metagenomeFeatures")

    demo_tree_file <- system.file("extdata", "mockTree.rds",
                                  package="metagenomeFeatures")

    demoMgDb <- new("MgDb",
                    seq = db_seq,
                    taxa_file = demo_taxa_file,
                    tree_file = demo_tree_file,
                    metadata = metadata)
}
