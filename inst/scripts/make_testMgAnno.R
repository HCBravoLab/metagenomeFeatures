## code to generate testMgAnno
library(metagenomeFeatures)
library(ShortRead)
library(Biostrings)

db_seq <- readDNAStringSet("../../tests/testSeq.fasta.gz")
metadata <- list(ACCESSION_DATE = "3/31/2015",
                 URL = "https://greengenes.microbio.me",
                 DB_TYPE_NAME = "GreenGenes",
                 DB_TYPE_VALUE = "MgDb",
                 DB_SCHEMA_VERSION = "1.0")

testMgDb <- new("MgDb", seq = db_seq,
                taxa = "../../tests/testTaxa.sqlite3",
                metadata = metadata)
query_subset <- sread(mgQuery[1:100])
query_ids <- taxa_keys(testMgDb,keytype = "Keys")$Keys[1:100]
testMgAnno <- annotate(testMgDb, db_keys = query_ids, query_seq = query_subset)
saveRDS(testMgAnno, "../../tests/testMgAnno.rds")
