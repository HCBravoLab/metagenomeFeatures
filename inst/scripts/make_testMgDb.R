### generating files for test MgDb

library(dplyr)
library(Biostrings)
library(DECIPHER)
library(ape)

test_keys <- as.character(1:10)
tax_names <- matrix(paste0("tax_",0:69), ncol = 7)
colnames(tax_names) <- c("Kingdom","Phylum","Class","Ord",
                         "Family","Genus","Species")
tax_names <- as.data.frame(tax_names)

taxa <- data.frame(Keys = test_keys, tax_names)

## Test seq data
set.seed <- 10
rand <- sapply(1:10, function(x) paste(sample(c("A","T","G","C"), 21, replace = T), collapse = ""))
test_seq <- DNAStringSet(rand)
names(test_seq) <- test_keys
saveRDS(test_seq,file = "../../tests/test_seq.rds")

## Create Test DB
db_file <- "../../tests/test_db.sqlite3"
db_conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)

DECIPHER::Seqs2DB(seqs = test_seq, type = "DNAStringSet", dbFile = db_conn, identifier = "MgDb")

### Adding taxonomic data to database
DECIPHER::Add2DB(myData = taxa, dbFile = db_conn)

## test tree data
f_name = tempfile()
writeLines("((1,(2,3,4),5),((6,7,8),9),10);",con = f_name)
test_phylo <- ape::read.tree(f_name)
saveRDS(test_phylo,file = "../../tests/test_tree.rds")

# Note random seq and tree assignments not related to taxa or seqs
