### Code to generate source files for a MgDb-object with data from the
### Greengenes 13.8 database 97% OTUs
library(DECIPHER)
library(Biostrings)
library(metagenomeFeatures) ## Needed for the make_mgdb function

## Database URL
db_root_url <- "ftp://greengenes.microbio.me/greengenes_release/gg_13_8_otus/"
taxa_url <- paste0(db_root_url, "taxonomy/97_otu_taxonomy.txt")
aligned_seq_url <- paste0(db_root_url, "rep_set_aligned/97_otus.fasta")
seq_url <- paste0(db_root_url, "rep_set/97_otus.fasta")
tree_url <- paste0(db_root_url, "trees/97_otus_unannotated.tree")

## Downloaded files
taxa_file <- tempfile()
seq_file <- tempfile()
tree_file <- "../extdata/gg13.8_97.tre"

## MD5 check sums from initial download
taxa_md5 <- "56ef15dccf2e931ec173f4f977ed649b"
seq_md5 <- "50b2269712b3738afb41892bed936c29"
tree_md5 <- "e42c0cb6e4eaf641742cd5767c4a0101"

## MgDb database files name
db_file <- "../extdata/gg13.8_97.sqlite"
metadata_file <- "../extdata/gg13.8_97_metadata.RData"

### Download database files ####################################################
download_db <- function(url, file_name, md5){
    ## Downloade file and check to make sure MD5 checksum matches checksum for
    ## previously downloaded version

    download.file(url,file_name)
    new_md5 <- tools::md5sum(file_name)
    if (md5 != new_md5) warning("checksum does not match downloaded file.")
}

## Taxa Data
download_db(taxa_url, taxa_file, taxa_md5)

## Seq Data
download_db(seq_url, seq_file, seq_md5)

## Tree Data
download_db(tree_url, tree_file, tree_md5)

### Create SQLite DB with Taxa and Seq Data ####################################
### Parse greengenes taxonomy
parse_greengenes <- function(taxonomy_file){
    taxa <- read.delim(taxonomy_file, stringsAsFactors = FALSE, header = FALSE)
    keys <- taxa[,1]
    taxa <- strsplit(taxa[,2],split = "; ")
    taxa <- t(sapply(taxa,function(i){i}))
    taxa <- cbind(keys,taxa)
    colnames(taxa) <- c("Keys","Kingdom","Phylum","Class","Ord","Family","Genus","Species")

    ## Return as a data.frame
    data.frame(taxa)
}

taxa_tbl <- parse_greengenes(taxa_file)

seqs <- Biostrings::readDNAStringSet(seq_file)

metagenomeFeatures::make_mgdb(db_name = "greengenes13.8_97",
                              db_file = db_file,
                              taxa_tbl = taxa_tbl,
                              seqs = seqs)



### Database Metadata ##########################################################
metadata <- list(ACCESSION_DATE = date(),
                 URL = "ftp://greengenes.microbio.me/greengenes_release/gg_13_8_otus",
                 DB_TYPE_NAME = "GreenGenes",
                 DB_VERSION = "13.8 97% OTUS",
                 DB_TYPE_VALUE = "MgDb",
                 DB_SCHEMA_VERSION = "2.0")

save("metadata", file = metadata_file)
