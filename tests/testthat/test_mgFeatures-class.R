## testing mgFeatures class
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

context("mgFeatures-class")
## mgF new ---------------------------------------------------------------------
## testing for errors when generating new metagenomeFeatures object
test_that("mgFeatures-class-new",{
    expect_is(new("mgFeatures",
                  listData = test_taxa,
                  metadata = test_metadata,
                  refDbSeq = test_seq,
                  refDbTree = test_tree), "mgFeatures")
    expect_is(new("mgFeatures",
                  listData = test_taxa,
                  metadata = test_metadata,
                  refDbSeq = test_seq,
                  refDbTree = NULL), "mgFeatures")

    # expect_warning(new("mgFeatures",
    #                  metadata = test_metadata,
    #                  refDbSeq = test_seq,
    #                  refDbTree = test_tree), "No taxonomic data provided")
    #
    # expect_message(new("mgFeatures",
    #                  data = test_taxa,
    #                  refDbSeq = test_seq,
    #                  refDbTree = test_tree), "No metadata data provided")
    #
    # expect_message(new("mgFeatures",
    #                  data = test_taxa,
    #                  metadata = test_metadata,
    #                  refDbTree = test_tree),"No sequence data provided")
    #
    # expect_message(new("mgFeatures",
    #                    data = test_taxa,
    #                    metadata = test_metadata,
    #                    refDbSeq=test_seq),"No tree provided")
    #
    # expect_error(new("mgFeatures",
    #                    metadata = test_metadata,
    #                    refDbSeq = "",
    #                    refDbTree = test_tree))
    #
    # expect_error(new("mgFeatures",
    #                  metadata = "",
    #                  refDbSeq = test_seq,
    #                  refDbTree = test_tree))
})

test_that("mgFeatures-class-constructor",{
    expect_is(mgFeatures(
        taxa = test_taxa,
        meta = test_metadata,
        seq = test_seq,
        tree = test_tree), "mgFeatures")
    expect_is(mgFeatures(
        taxa = test_taxa,
        meta = test_metadata,
        seq = test_seq,
        tree = NULL), "mgFeatures")
})

## mgF slots -------------------------------------------------------------------
test_that("mgFeatures-class-slots",{
    expect_is(test_mgF, "mgFeatures")
    expect_identical(test_mgF@listData, as.list(test_taxa))
    expect_identical(test_mgF@metadata, test_metadata)
    expect_identical(test_mgF@refDbSeq, test_seq)
    expect_identical(test_mgF@refDbTree, test_tree)
})

## mgF accessors ---------------------------------------------------------------
test_that("mgFeatures-accessors",{
    expect_identical(mgF_taxa(test_mgF), DataFrame(test_mgF))
    expect_identical(mgF_meta(test_mgF), test_mgF@metadata)
    expect_identical(mgF_seq(test_mgF), test_mgF@refDbSeq)
    expect_identical(mgF_tree(test_mgF), test_mgF@refDbTree)
})

## mgF subset ------------------------------------------------------------------
test_that("mgFeature-subset", {
    ## Checking the subset function works for numeric and text based subsets
    expect_s4_class(test_mgF[1:5,],"mgFeatures")
    expect_s4_class(test_mgF[rownames(test_mgF) %in% 1:5,], "mgFeatures")
    expect_equal(test_mgF[1:5,], test_mgF[test_mgF$Keys %in% 1:5,])

    ## Checking individual slots were subset correctly
    subset_test_mgF <- test_mgF[1:5,]
    expect_equal(subset_test_mgF@listData, as.list(test_taxa[1:5,]))

    expect_equal(subset_test_mgF@refDbTree,
                 drop.tip(test_tree,tip = as.character(6:10)))
    expect_equal(subset_test_mgF@refDbSeq, test_seq[1:5])

})


