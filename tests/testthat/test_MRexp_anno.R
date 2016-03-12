# ## annotating MRexp
library(metagenomeFeatures)
library(dplyr)
library(ape)
library(metagenomeSeq)

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

count_dat <- data.frame(sam1 = c(1:5))
OTUcenters <- as.character(1:5)
rownames(count_dat) <- OTUcenters

testMRexp = newMRexperiment(counts = count_dat)

context("annotateMRexp_fData")

test_that("MgDb-class annotateMRexp",{
    expect_silent(test_anno <- annotateMRexp_fData(
        mgdb = testMgDb, MRobj = testMRexp))

    expect_is(featureData(test_anno), "mgFeatures")
})
