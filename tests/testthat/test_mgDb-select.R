context("mgDb-select")

## Select Methods
test_that("MgDb-class select arguments",{
    expect_error(mgDb_select(testMgDb, type = "not a type"))
    expect_error(mgDb_select(testMgDb, type = "all", keytype = "Keys"))
    expect_error(mgDb_select(testMgDb, type = "all", keys = "Streptomyces"))
})

test_that("MgDb-class select return",{
    expect_equal(mgDb_select(testMgDb, type = c("seq","tree")),
                 list(seq = testMgDb$seq, tree = testMgDb$tree))

    expect_equal(mgDb_select(testMgDb, type = c("taxa","tree")),
                 list(taxa = testMgDb$taxa, tree = testMgDb$tree))

    expect_equal(mgDb_select(testMgDb, type = c("seq","tree")),
                 list(seq = testMgDb$seq, tree = testMgDb$tree))

    expect_equal(mgDb_select(testMgDb, type = "all"),
                 list(taxa = testMgDb$taxa,
                      seq = testMgDb$seq,
                      tree = testMgDb$tree))
})

## TODO add test for Greengenes format


test_that("MgDb-class select taxa",{
    test_select_taxa <- mgDb_select(
        testMgDb,type = "taxa",
        keys = c("tax_51", "tax_53"),
        keytype = "Genus")

    ## test type
    expect_is(test_select_taxa,"tbl_df")

    ## test value
    expect_equal(test_select_taxa, taxa[c(2,4),])
})


test_that("MgDb-class select seq", {
    test_select_seq <- mgDb_select(
        testMgDb,type = "seq",
        keys = c("tax_51", "tax_53"),
        keytype = "Genus")

    ## test type
    expect_is(test_select_seq, "DNAStringSet")

    ## test value
    expect_equal( test_select_seq, test_seq[c(2,4)])
})



test_that("MgDb-class select tree", {
    test_select_tree <- mgDb_select(
        testMgDb,type = "tree",
        keys = c("tax_51", "tax_53"),
        keytype = "Genus")

    ## test type
    expect_is(test_select_tree, "phylo")

    ## test value
    expect_equal(test_select_tree,
                 drop.tip(test_tree,
                          tip = test_tree$tip.label[-c(2,4)]))
})



test_that("MgDb-class select all",{
    test_select_all <- mgDb_select(
        testMgDb,type = "all",
        keys = c("tax_51", "tax_53"),
        keytype = "Genus")


    # expect_equal_to_reference(
    #     test_select_all,
    #     file = "cache/MgDb_test_select_all.rds")

    ## Test type
    expect_is(test_select_all, "list")
    expect_is(test_select_all$taxa, "tbl_df")
    expect_is(test_select_all$seq, "DNAStringSet")
    expect_is(test_select_all$tree, "phylo")

    ## test value taxa
    test_select_taxa <- mgDb_select(
        testMgDb,type = "taxa",
        keys = c("tax_51", "tax_53"),
        keytype = "Genus")

    expect_equal(test_select_all$taxa, test_select_taxa)

    ## test value seq
    test_select_seq <- mgDb_select(
        testMgDb,type = "seq",
        keys = c("tax_51", "tax_53"),
        keytype = "Genus")

    expect_equal(test_select_all$seq, test_select_seq)

    ## test value tree
    test_select_tree <- mgDb_select(
        testMgDb,type = "tree",
        keys = c("tax_51", "tax_53"),
        keytype = "Genus")

    expect_equal(test_select_all$tree, test_select_tree)
})
