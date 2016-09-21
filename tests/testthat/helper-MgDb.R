library(dplyr)
library(ape)
library(metagenomeFeatures)

test_metadata <- list(ACCESSION_DATE = "1/11/1111",
                      URL = "test-data",
                      DB_TYPE_NAME = "Test",
                      DB_TYPE_VALUE = "MgDb",
                      DB_SCHEMA_VERSION = "1.0")

test_taxa_file <- "../test_taxa.sqlite3"

test_seq_file <- "../test_seq.rds"
test_seq <- readRDS(test_seq_file)

test_tree_file <- "../test_tree.rds"
test_tree <- readRDS(test_tree_file)

testMgDb <- new("MgDb", seq = test_seq,
                taxa_file = test_taxa_file,
                tree_file = test_tree_file,
                metadata = test_metadata)


## data.frame for testing select methods
tax_names <- matrix(paste0("tax_",0:69), ncol = 7)
colnames(tax_names) <- c("Kingdom","Phylum","Class","Order",
                         "Family","Genus","Species")
# tax_names <- as.data.frame(tax_names, stringsAsFactors = FALSE)

taxa <- data.frame(Keys = as.character(1:10), tax_names,
                   stringsAsFactors = FALSE) %>% dplyr::as_data_frame()


data(msd16s_query_df)

OTU <- c(4,4,4,3,2)

incomplete_query_df <- data.frame(OTU)

Keys_not <- c(1,1,1,1,1)

wrong_query_df <- data.frame(Keys_not, OTU)

Keys <- c(1,2,3,4,5)

query_df <- data.frame(Keys, OTU)
