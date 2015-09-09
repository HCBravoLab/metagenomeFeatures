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
query_subset <- mgQuery[1:100]
testMgAnno <- annotate(testMgDb, query = query_subset, mapping = "arbitrary")
saveRDS(testMgAnno, "../../tests/testMgAnno.rds")
