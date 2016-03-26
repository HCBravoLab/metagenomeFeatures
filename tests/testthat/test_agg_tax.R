context("aggregateByTaxonomy")

## agg_taxa fails if only one sample is present in count, unable to assign row names for n X 1 matrix

test_that("aggregateByTaxonomy-return-class",{
    expect_is(aggregateByTaxonomy(test_MRexp, lvl = "Species"),"MRexperiment")
    expect_is(aggregateByTaxonomy(test_MRexp, lvl = "Species",
                                  out = "matrix"),"matrix")
})

## tests for wrong input

## test for wrong params
test_that("aggregateByTaxonomy-params", {
    expect_error(aggregateByTaxonomy("not MRexp", lvl = "Species"))
    expect_error(aggregateByTaxonomy(test_MRexp, lvl = 10))
    expect_error(aggregateByTaxonomy(test_MRexp, lvl = "NOT a level"))
    expect_error(aggregateByTaxonomy(test_MRexp, lvl = "Species", out = "Wrong!"))
    expect_error(aggregateByTaxonomy(test_MRexp, lvl = "Species", alternate = "BAD"))
    expect_error(aggregateByTaxonomy(test_MRexp, lvl = "Species", norm = "BAD"))
    expect_error(aggregateByTaxonomy(test_MRexp, lvl = "Species", log = "BAD"))
    # expect_error(aggregateByTaxonomy(test_MRexp, lvl = "Species", aggFun = "BAD"))  # no current check, how to check function is appropriate for matrix?
    expect_warning(aggregateByTaxonomy(test_MRexp, lvl = "Species", sl = "BAD"))  # no current check
})


## test for single sample
test_that("aggregateByTaxonomy-single sample", {
    expect_error(aggregateByTaxonomy(test_MRexp1, lvl = "Species"))
})

## test for undefined taxa (NA)


## tests for count values


## test for all unique
test_that("aggregateByTaxonomy-single sample", {
    expect_message(aggregateByTaxonomy(test_MRexp_all_unique, lvl = "Species"))
    ## need to think about best test dataset for when the user specified level has only unique values
    ## expect_message(aggregateByTaxonomy(test_MRexp_lvl_unique, lvl = "Species"))
})

## test for taxa_levels
