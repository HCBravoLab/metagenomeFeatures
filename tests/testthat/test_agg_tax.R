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

test_that("aggregate_taxa-return-class",{
    expect_is(aggregate_taxonomy(testMgDb, taxa_level = "Species")[["agg_taxa_table"]],"tbl")
    expect_is(aggregate_taxonomy(testMgDb, taxa_level = "Species", mapping = TRUE)[["agg_taxa_mapping"]],"tbl")
})

## tests for wrong input

## test for wrong params
test_that("aggregate_taxa-params", {
    expect_error(aggregate_taxa("not MRexp", taxa_level = "Species"))
    expect_error(aggregate_taxa(testMgDb, taxa_level = 10))
    expect_error(aggregate_taxa(testMgDb, taxa_level = "NOT a level"))
    expect_error(aggregate_taxa(testMgDb, taxa_level = "Species", mapping = "Wrong!"))
    ##  no current check, how to check function is appropriate for matrix?
    # expect_error(aggregate_taxa(test_MRexp, lvl = "Species", aggFun = "BAD"))
    ## silent arguments - move checks to MRcounts
    # expect_warning(aggregate_taxa(test_MRexp, lvl = "Species", sl = "BAD"))
    # expect_error(aggregate_taxa(test_MRexp, lvl = "Species", norm = "BAD"))
    # expect_error(aggregate_taxa(test_MRexp, lvl = "Species", log = "BAD"))
})

test_that("aggregate_taxa_mgf-return-class",{
    expect_is(aggregate_taxonomy_mgf(test_mgF, taxa_level = "Species")[["agg_taxa_table"]],"mgFeatures")
    expect_is(aggregate_taxonomy_mgf(test_mgF, taxa_level = "Species", mapping = TRUE)[["agg_taxa_mapping"]],"mgFeatures")
})

## tests for wrong input

# ## test for wrong params
# test_that("aggregate_taxa-params", {
#     expect_error(aggregate_taxa("not MRexp", taxa_level = "Species"))
#     expect_error(aggregate_taxa(testMgDb, taxa_level = 10))
#     expect_error(aggregate_taxa(testMgDb, taxa_level = "NOT a level"))
#     expect_error(aggregate_taxa(testMgDb, taxa_level = "Species", mapping = "Wrong!"))
#     ##  no current check, how to check function is appropriate for matrix?
#     # expect_error(aggregate_taxa(test_MRexp, lvl = "Species", aggFun = "BAD"))
#     ## silent arguments - move checks to MRcounts
#     # expect_warning(aggregate_taxa(test_MRexp, lvl = "Species", sl = "BAD"))
#     # expect_error(aggregate_taxa(test_MRexp, lvl = "Species", norm = "BAD"))
#     # expect_error(aggregate_taxa(test_MRexp, lvl = "Species", log = "BAD"))
# })
#
#
# ## test for single sample
# test_that("aggregate_taxa-single sample", {
#     expect_error(aggregate_taxa(test_MRexp1, lvl = "Species"))
# })
#
# ## test for undefined taxa (NA)
#
#
#
# ## tests for count values
# test_that("aggregate_taxa-matrix value", {
#     exp_colnames <- paste0("sam",1:2)
#
#     ## check Species level agg
#     exp_rownames <- paste0("tax_",c(60,62:69))
#     exp_mat <- matrix(data = c(3,3:10,5,(3:10)^2), ncol = 2,
#                       dimnames = list(exp_rownames, exp_colnames))
#     expect_equal(aggregate_taxa(test_MRexp, lvl = "Species", out = "matrix"),
#                  exp_mat)
#
#     ## check Genus level agg
#     exp_rownames <- paste0("tax_",c(50,52,54:59))
#     exp_mat <- matrix(data = c(3,7,5:10,5,25,(5:10)^2), ncol = 2,
#                       dimnames = list(exp_rownames, exp_colnames))
#     expect_equal(aggregate_taxa(test_MRexp, lvl = "Genus", out = "matrix"),
#                  exp_mat)
#
#     ## check Family level agg
#     exp_rownames <- paste0("tax_",c(40,42,44,46:49))
#     exp_mat <- matrix(data = c(3,7,11,7:10,5,25,61,(7:10)^2), ncol = 2,
#                       dimnames = list(exp_rownames, exp_colnames))
#     expect_equal(aggregate_taxa(test_MRexp, lvl = "Family", out = "matrix"),
#                  exp_mat)
#
#     ## add tests for other maxtrix sum opperations
# })
#
# ## test for all unique
# test_that("aggregate_taxa-single sample", {
#     expect_message(aggregate_taxa(test_MRexp_all_unique, lvl = "Species"))
#     ## need to think about best test dataset for when the user specified level has only unique values
#     ## expect_message(aggregate_taxa(test_MRexp_lvl_unique, lvl = "Species"))
# })
#
# ## test for taxa_levels
# test_that("taxa_levels", {
#     expect_is(taxa_levels(test_MRexp), "character")
#     expect_error(taxa_levels("not mr_exp"))
#     exp_val <- c("OTU","Kingdom","Phylum","Class","Order","Family","Genus","Species")
#     expect_equal(taxa_levels(test_MRexp), exp_val)
# })
