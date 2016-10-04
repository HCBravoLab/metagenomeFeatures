## hidden functions for vignette data
vignette_assay_data <- function(){
    system.file("extdata", "mock_counts.csv",
                package="metagenomeFeatures") %>%
        metagenomeSeq::load_meta(sep = ",")
}


vignette_pheno_data <- function(){
    system.file("extdata", "mock_sample_data.csv",
                package="metagenomeFeatures")  %>%
        read.csv(row.names = 1, stringsAsFactors = F) %>%
        Biobase::AnnotatedDataFrame()
}

vignette_mock_MgDb <- function(){
    metadata <- list(ACCESSION_DATE = "3/31/2015",
                     URL = "https://greengenes.microbio.me",
                     DB_TYPE_NAME = "GreenGenes-MgDb-Mock",
                     DB_TYPE_VALUE = "MgDb",
                     DB_SCHEMA_VERSION = "1.0")

    db_seq <- system.file("extdata", "mock_MgDb_seq.fasta.gz",
                          package="metagenomeFeatures") %>%
        Biostrings::readDNAStringSet()

    new("MgDb",
            seq = db_seq,
            taxa_file = system.file("extdata", "mock_MgDb_taxa.sqlite",
                                    package="metagenomeFeatures"),
            tree_file = "not available",
            # tree_file = system.file("extdata","msd16s_MgDb_tree.rds",
            #                        package="metagenomeFeatures"),
            metadata = metadata)
}
