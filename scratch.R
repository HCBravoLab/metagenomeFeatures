library(metagenomeFeatures)

## test generate MgDb object
load("../../annotation/annotation/query.rdata")
metadata <- list(ACCESSION_DATE = "3/31/2015",
                 URL = "https://greengenes.microbio.me",
                 DB_TYPE_NAME = "GreenGenes",
                 DB_TYPE_VALUE = "MgDb",
                 DB_SCHEMA_VERSION = "1.0")

testMgDb <- new("MgDb",seq = query, taxa = "../../annotation/annotation/inst/extdata/taxaDb.sqlite3", metadata = metadata)

testMgDb

## testing taxa_keytypes
taxa_keytypes(testMgDb)

taxa_columns(testMgDb)

head(taxa_keys(testMgDb, keytype = c("Kingdom")))

## testing select methods
testMgDb$select(testMgDb,
                type = "taxa",
                keys = c("Vibrio", "Salmonella"),
                keytype = "Genus")

# -- still need to workout seq select
# testMgDb$select(testMgDb,
#                 type = "seq",
#                 keys = c("Vibrio", "Salmonella"),
#                 keytype = "Genus")


# #testMgDb$select(testMgDb,
#                 type = "both",
#                 keys = c("Vibrio", "Salmonella"),
#                 keytype = "Genus")

### metagenomeAnnotation class

## testing annotate method
