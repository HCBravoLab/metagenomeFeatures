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
test_that("aggregateByTaxonomy-matrix value", {
    exp_colnames <- paste0("sam",1:2)

    ## check Species level agg
    exp_rownames <- paste0("tax_",c(60,62:69))
    exp_mat <- matrix(data = c(3,3:10,5,(3:10)^2), ncol = 2,
                          dimnames = list(exp_rownames, exp_colnames))
    expect_equal(aggregateByTaxonomy(test_MRexp, lvl = "Species", out = "matrix"), exp_mat)

    ## check Genus level agg
    exp_rownames <- paste0("tax_",c(50,52,54:59))
    exp_mat <- matrix(data = c(3,7,5:10,5,25,(5:10)^2), ncol = 2,
                          dimnames = list(exp_rownames, exp_colnames))
    expect_equal(aggregateByTaxonomy(test_MRexp, lvl = "Genus", out = "matrix"), exp_mat)

    ## check Family level agg
    exp_rownames <- paste0("tax_",c(40,42,44,46:49))
    exp_mat <- matrix(data = c(3,7,11,7:10,5,25,61,(7:10)^2), ncol = 2,
                      dimnames = list(exp_rownames, exp_colnames))
    expect_equal(aggregateByTaxonomy(test_MRexp, lvl = "Family", out = "matrix"), exp_mat)

    ## add tests for other maxtrix sum opperations
})

## test for all unique
test_that("aggregateByTaxonomy-single sample", {
    expect_message(aggregateByTaxonomy(test_MRexp_all_unique, lvl = "Species"))
    ## need to think about best test dataset for when the user specified level has only unique values
    ## expect_message(aggregateByTaxonomy(test_MRexp_lvl_unique, lvl = "Species"))
})

## test for taxa_levels
test_that("taxa_levels", {
    expect_is(taxa_levels(test_MRexp), "character")
    expect_error(taxa_levels("not mr_exp"))
    exp_val <- c("OTU","Kingdom","Phylum","Class","Order","Family","Genus","Species")
    expect_equal(taxa_levels(test_MRexp), exp_val)
})
