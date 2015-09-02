library(metagenomeFeatures)
library(Biostrings)
## test generate MgDb object
db_seq <- readDNAStringSet("../testSeq.fasta.gz")
metadata <- list(ACCESSION_DATE = "3/31/2015",
                 URL = "https://greengenes.microbio.me",
                 DB_TYPE_NAME = "GreenGenes",
                 DB_TYPE_VALUE = "MgDb",
                 DB_SCHEMA_VERSION = "1.0")

testMgDb <- new("MgDb", seq = db_seq,
                taxa = "../testTaxa.sqlite3",
                metadata = metadata)

context("mgDb-class")

## uses expect_equal_to_reference, caches the results from the first time it is run and compares to future runs
test_that("MgDb-class show", {
    expect_equal_to_reference(show(testMgDb),
                              file = "cache/MgDb_test_show.rds")
})

test_that("MgDb-class taxa_keytypes", {
    expect_equal_to_reference(taxa_keytypes(testMgDb),
                              file = "cache/MgDb_test_taxa_keytypes.rds")
})

test_that("MgDb-class taxa_columns", {
    expect_equal_to_reference(taxa_columns(testMgDb),
                              file = "cache/MgDb_test_taxa_columns.rds")
})

test_that("MgDb-class taxa_keytypes and taxa_columns are identical", {
    expect_identical(taxa_columns(testMgDb), taxa_keytypes(testMgDb))
})

test_that("MgDb-class taxa_keys at different taxonomic levels", {
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Kingdom")),
                              file = "cache/MgDb_test_taxa_keys_Kingdom.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Phylum")),
                              file = "cache/MgDb_test_taxa_keys_Phylum.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Class")),
                              file = "cache/MgDb_test_taxa_keys_Class.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Order")),
                              file = "cache/MgDb_test_taxa_keys_Order.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Family")),
                              file = "cache/MgDb_test_taxa_keys_Family.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Genus")),
                              file = "cache/MgDb_test_taxa_keys_Genus.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Species")),
                              file = "cache/MgDb_test_taxa_keys_Species.rds")
})


## Select Methods
test_that("MgDb-class select methods",{
    expect_equal_to_reference(
        testMgDb$select(type = "taxa",
                        keys = c("Vibrio", "Salmonella"),
                        keytype = "Genus"),
        file = "cache/MgDb_test_select_taxa.rds")
    expect_equal_to_reference(
        testMgDb$select(type = "seq",
                        keys = c("Vibrio", "Salmonella"),
                        keytype = "Genus"),
        file = "cache/MgDb_test_select_seq.rds")
    expect_equal_to_reference(
        testMgDb$select(type = "both",
                        keys = c("Vibrio", "Salmonella"),
                        keytype = "Genus"),
        file = "cache/MgDb_test_select_both.rds")
})

test_that("MgDb-class annotate",{
    expect_equal_to_reference(
        testMgDb$annotate(query = mgQuery[1:20],
                          mapping = "arbitrary"),
        file = "cache/MgDb_test_annotate.rds")
})
