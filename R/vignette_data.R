## hidden functions for vignette data
assay_data <- function(){
    file.path(dataDirectory, "msd16s_counts.csv") %>% metagenomeSeq::load_meta(sep = ",")
}


pheno_data <- function(){
    file.path(dataDirectory, "msd16s_sample_data.csv") %>%
        read.csv(row.names = 1, stringsAsFactors = F) %>%
        Biobase::AnnotatedDataFrame()
}

msd16s_MgDb <- function(){
    metadata <- list(ACCESSION_DATE = "3/31/2015",
                     URL = "https://greengenes.microbio.me",
                     DB_TYPE_NAME = "GreenGenes-MgDb-msd16s",
                     DB_TYPE_VALUE = "MgDb",
                     DB_SCHEMA_VERSION = "1.0")

    db_seq <- file.path(dataDirectory,"msd16s_MgDb_seq.fasta.gz") %>%
        readDNAStringSet()

    new("MgDb",seq = db_seq,
            taxa_file = file.path(dataDirectory,"msd16s_MgDb_taxa.sqlite"),
            tree_file = file.path(dataDirectory,"msd16s_MgDb_tree.rds"),
            metadata = metadata)
}
