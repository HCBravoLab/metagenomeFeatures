## testing mgFeatures class
library(magrittr)
library(ape)

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
    as.data.frame(tax_names) %>%
        data.frame(Keys = test_keys, .)
}

test_taxa <- make_test_taxa()

test_seq <- readRDS("../test_seq.rds")
test_tree <- readRDS("../test_tree.rds") %>% as(Class = "phylo")




context("mgFeatures-class")
## mgF new ---------------------------------------------------------------------
## testing for errors when generating new metagenomeFeatures object
test_that("mgFeatures-class-new",{
    expect_is(new("mgFeatures",
                  data = test_taxa,
                  metadata = test_metadata,
                  refDbSeq = test_seq,
                  refDbTree = test_tree),
              "mgFeatures")

    # expect_warning(new("mgFeatures",
    #                  metadata = test_metadata,
    #                  refDbSeq = test_seq,
    #                  refDbTree = test_tree),"No taxonomic data provided")
    #
    # expect_message(new("mgFeatures",
    #                  data = test_taxa,
    #                  refDbSeq = test_seq,
    #                  refDbTree = test_tree),"No metadata data provided")
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

    expect_error(new("mgFeatures",
                       metadata = test_metadata,
                       refDbSeq = "",
                       refDbTree = test_tree))

    expect_error(new("mgFeatures",
                     metadata = "",
                     refDbSeq = test_seq,
                     refDbTree = test_tree))

    expect_error(new("mgFeatures",
                     metadata = test_metadata,
                     refDbSeq = test_seq,
                     refDbTree = ""))
})


## mgF slots -------------------------------------------------------------------
test_that("mgFeatures-class-slots",{
    test_mgF <- new("mgFeatures",
                   data = test_taxa,
                   metadata = test_metadata,
                   refDbSeq = test_seq,
                   refDbTree = test_tree)
    ## test slots
    expect_is(test_mgF, "mgFeatures")
    expect_identical(test_mgF@data, test_taxa)
    expect_identical(test_mgF@metadata, test_metadata)
    expect_identical(test_mgF@refDbSeq, test_seq)
    expect_identical(test_mgF@refDbTree, test_tree)
})

## mgF accessors ---------------------------------------------------------------
test_that("mgFeatures-accessors",{
    test_mgF <- new("mgFeatures",
                   data = test_taxa,
                   metadata = test_metadata,
                   refDbSeq=test_seq,
                   refDbTree = test_tree)

    expect_identical(mgF_taxa(test_mgF), test_mgF@data)
    expect_identical(mgF_meta(test_mgF), test_mgF@metadata)
    expect_identical(mgF_seq(test_mgF), test_mgF@refDbSeq)
    expect_identical(mgF_tree(test_mgF), test_mgF@refDbTree)
})

## test for subsetting
