library(ape)
library(S4Vectors)

test_metadata <- list(ACCESSION_DATE = "1/11/1111",
                      URL = "test-data",
                      DB_TYPE_NAME = "Test",
                      DB_TYPE_VALUE = "MgDb",
                      DB_SCHEMA_VERSION = "1.0")

make_test_taxa <- function(){
    test_keys <- as.character(1:10)

    tax_names <- matrix(paste0("tax_",0:69), ncol = 7)
    colnames(tax_names) <- c("Kingdom","Phylum","Class","Order",
                             "Family","Genus","Species")
    df <- as.data.frame(tax_names)
    data.frame(Keys = test_keys, df)
}

test_taxa <- make_test_taxa()

test_seq <- readRDS("../test_seq.rds")
test_tree <- as(readRDS("../test_tree.rds"), Class = "phylo")

test_mgF <- new("mgFeatures",
                DataFrame(test_taxa, row.names = test_taxa$Keys),
                metadata = test_metadata,
                refDbSeq = test_seq,
                refDbTree = test_tree)

context("aggregate_taxa")

## agg_taxa fails if only one sample is present in count, unable to assign row names for n X 1 matrix

test_that("mgDb_aggregate_taxa-return-class",{
    expect_is(mgDb_aggregate_taxonomy(testMgDb, taxa_level = "Species"),"tbl")
    expect_is(mgDb_aggregate_taxonomy(testMgDb, taxa_level = "Species", mapping = TRUE)[["agg_taxa_mapping"]],"tbl")
})

## tests for wrong input

## test for wrong params
test_that("mgDb_aggregate_taxa-params", {
    expect_error(mgDb_aggregate_taxa("not MgDb", taxa_level = "Species"))
    expect_error(mgDb_aggregate_taxa(testMgDb, taxa_level = 10))
    expect_error(mgDb_aggregate_taxa(testMgDb, taxa_level = "NOT a level"))
    expect_error(mgDb_aggregate_taxa(testMgDb, taxa_level = "Species", mapping = "Wrong!"))
})

test_that("mgF_aggregate_taxa-return-class",{
    expect_is(mgF_aggregate_taxonomy(test_mgF, taxa_level = "Species"),"DataFrame")
    expect_is(mgF_aggregate_taxonomy(test_mgF, taxa_level = "Species", mapping = TRUE)[["agg_taxa_mapping"]],"DataFrame")
})
