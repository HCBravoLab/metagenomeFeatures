context("mgDb-class")

test_that("MgDb-class",{
    expect_is(testMgDb, "MgDb")
    expect_is(newMgDb(db_file = test_db_file,
                      tree = NULL,
                      metadata = test_metadata),
              "MgDb")
})

## Not actually testing the show method
# test_that("MgDb-class show", {
#     expect_equal_to_reference(show(testMgDb),
#                               file = "cache/MgDb_test_show.rds")
# })

test_that("MgDb-class slots", {
    expect_is(testMgDb@metadata, "list")
    expect_equal(testMgDb@metadata, test_metadata)
})

test_that("MgDb-class seq",{
    expect_is(testMgDb@seq, "SQLiteConnection")
    ## Need to figure out a better test
    # expect_equal(testMgDb@seq, test_seq)
})

test_that("MgDb-class taxa",{
    expect_is(testMgDb@taxa, "tbl_dbi")
})

## Test fails due to additional columns in table from seqDB - not sure the best
## way to address this issue.
taxa_levels = c("Keys","Kingdom","Phylum","Class","Ord","Family","Genus","Species")
test_that("MgDb-class taxa_keytypes", {
    expect_equal(taxa_keytypes(testMgDb), taxa_levels)
    expect_identical(length(taxa_keytypes(testMgDb)), length(taxa_levels))
})

test_that("MgDb-class taxa_columns", {
    expect_equal(taxa_columns(testMgDb), taxa_levels)
})

test_that("MgDb-class taxa_keytypes and taxa_columns are identical", {
    expect_identical(taxa_columns(testMgDb), taxa_keytypes(testMgDb))
})

test_that("MgDb-class taxa_keys at different taxonomic levels", {
    expect_error(taxa_keys(testMgDb, keytype = c("not a type")))
    expect_error(taxa_keys(testMgDb, keytype = c("not a type","neither is this")))
    expect_error(taxa_keys(testMgDb, keytype = c("not a type","Kingom")))
    expect_equal(taxa_keys(testMgDb, keytype = c()), testMgDb@taxa)

    i <- 0
    for (lvl in taxa_levels[-1]) {
        expect_equal(taxa_keys(testMgDb, keytype = c(lvl)) %>% .[[lvl]],
                     paste0("tax_",i:(i + 9)))
        i <- i + 10
    }
})




## MgDb accessors ---------------------------------------------------------------
test_that("MgDb-accessors",{
    expect_identical(mgdb_taxa(testMgDb), testMgDb@taxa)
    expect_identical(mgdb_meta(testMgDb), testMgDb@metadata)
    expect_identical(mgdb_seq(testMgDb), testMgDb@seq)
    expect_identical(mgdb_tree(testMgDb), testMgDb@tree)
})

