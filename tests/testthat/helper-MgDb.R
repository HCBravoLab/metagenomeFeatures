library(dplyr)
library(ape)
library(metagenomeFeatures)

test_metadata <- list(ACCESSION_DATE = "1/11/1111",
                      URL = "test-data",
                      DB_TYPE_NAME = "Test",
                      DB_TYPE_VALUE = "MgDb",
                      DB_SCHEMA_VERSION = "2.0")

test_db_file <- "../test_db.sqlite3"


## Test seq data
# Creating DNAStringSet in test - to address build test error on devl branch
test_keys <- as.character(1:10)
set.seed <- 10
rand <- sapply(1:10, function(x) paste(sample(c("A","T","G","C"), 21, replace = T), collapse = ""))
test_seq <- Biostrings::DNAStringSet(rand)
names(test_seq) <- test_keys

test_tree_file <- "../test_tree.rds"
test_tree <- readRDS(test_tree_file)

## Update to work with new class definition
testMgDb <- newMgDb(db_file = test_db_file, tree = test_tree_file, metadata = test_metadata)



## data.frame for testing select methods
tax_names <- matrix(paste0("tax_",0:69), ncol = 7)
colnames(tax_names) <- c("Kingdom","Phylum","Class","Ord",
                         "Family","Genus","Species")
taxa <- data.frame(Keys = as.character(1:10), tax_names,
                   stringsAsFactors = FALSE)
taxa <- dplyr::as_data_frame(taxa)


OTU <- c(4,4,4,3,2)

incomplete_query_df <- data.frame(OTU)

Keys_not <- c(1,1,1,1,1)

wrong_query_df <- data.frame(Keys_not, OTU)

Keys <- c(1,2,3,4,5)

query_df <- data.frame(Keys, OTU)
