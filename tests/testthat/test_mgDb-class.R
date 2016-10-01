context("mgDb-class")

test_that("MgDb-class",{
    expect_is(testMgDb, "MgDb")

    expect_is(new("MgDb",
                  seq = test_seq,
                  taxa_file = test_taxa_file,
                  tree_file = "not available",
                  metadata = test_metadata), "MgDb")
})

## Not actually testing the show method
# test_that("MgDb-class show", {
#     expect_equal_to_reference(show(testMgDb),
#                               file = "cache/MgDb_test_show.rds")
# })

test_that("MgDb-class slots", {
    expect_is(testMgDb$metadata, "list")
    expect_equal(testMgDb$metadata, test_metadata)
})

test_that("MgDb-class seq",{
    expect_is(testMgDb$seq, "DNAStringSet")
    expect_equal(testMgDb$seq, test_seq)
})

test_that("MgDb-class taxa",{
    expect_is(testMgDb$taxa, "tbl_sqlite")
})

taxa_levels = c("Keys","Kingdom","Phylum","Class","Order","Family","Genus","Species")
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
    expect_equal(taxa_keys(testMgDb, keytype = c()), testMgDb$taxa)

    i <- 0
    for(lvl in taxa_levels[-1]){
        expect_equal(taxa_keys(testMgDb, keytype = c(lvl)) %>% .[[lvl]],
                     paste0("tax_",i:(i+9)))
        i <- i + 10
    }
})




## MgDb accessors ---------------------------------------------------------------
test_that("MgDb-accessors",{
    expect_identical(mgdb_taxa(testMgDb), testMgDb$taxa)
    expect_identical(mgdb_meta(testMgDb), testMgDb$metadata)
    expect_identical(mgdb_seq(testMgDb), testMgDb$seq)
    expect_identical(mgdb_tree(testMgDb), testMgDb$tree)
})

## test generate MgDb object
# db_seq <- Biostrings::readDNAStringSet("../testSeq.fasta.gz")
# metadata <- list(ACCESSION_DATE = "3/31/2015",
#                  URL = "https://greengenes.microbio.me",
#                  DB_TYPE_NAME = "GreenGenes",
#                  DB_TYPE_VALUE = "MgDb",
#                  DB_SCHEMA_VERSION = "1.0")
#
# taxdb_file <- "../testTaxa.sqlite3"
# test_db_keys <- c("4324716", "246960", "222675", "156874", "4383832")
# test_query_df <- data.frame(Keys = test_db_keys)
# test_query_seq = ShortRead::sread(mgQuery[1:5])
# test_annotate <- annotate(testMgDb, query_seq = test_query_seq,
#                           query_df = test_query_df)
# test_annotate_no_query_seq <-  annotate(testMgDb, db_keys = test_db_keys)
# test_that("MgDb-class annotate check arguments-query_seq only", {
#     expect_error(annotate(testMgDb, query_seq = test_query_seq))
# })
#
# test_that("MgDb-class annotate check arguments-query_df no Keys Column", {
#     expect_error(annotate(testMgDb, query_df = data.frame(NotKeys = test_db_keys)))
# })

# test_that("MgDb-class annotate check arguments-db_keys only", {
#     expect_message(annotate(testMgDb, db_keys = test_db_keys))
# })
# # ## Add tests for query_df
# # ##  make sure able to filter
# # ##  make sure added to dataframe
# #
# # test_that("MgDb-class annotate regression test",{
# #     expect_equal_to_reference(
# #         test_annotate,
# #         file = "cache/MgDb_test_annotate.rds")
# # })
#
# ## should this be moved to validity check?
# test_that("MgDb-class annotate test class types",{
#     expect_is(test_annotate, "metagenomeAnnotation")
#     expect_is(test_annotate@metadata, "list")
#     expect_is(test_annotate@experimentSeqData, "DNAStringSet")
# })
#
#
# test_that("MgDb-class annotate test selected data size",{
#     expect_equivalent(nrow(test_annotate),
#                       length(test_annotate@experimentSeqData))
#     expect_equivalent(length(test_annotate_no_query_seq@experimentSeqData), 0)
#     expect_equivalent(nrow(test_annotate_no_query_seq), 5)
# })
#
#
# test_that("MgDb-class annotate metadata", {
#     expect_equal(test_annotate@metadata$ACCESSION_DATE,
#                  testMgDb$metadata$ACCESSION_DATE)
#     expect_equal(test_annotate@metadata$URL,
#                  testMgDb$metadata$URL)
#     expect_equal(test_annotate@metadata$DB_TYPE_NAME,
#                  testMgDb$metadata$DB_TYPE_NAME)
#     expect_equal(test_annotate@metadata$DB_TYPE_VALUE,
#                  testMgDb$metadata$DB_TYPE_VALUE)
#     expect_equal(test_annotate@metadata$DB_SCHEMA_VERSION,
#                  testMgDb$metadata$DB_SCHEMA_VERSION)
#     expect_equal(test_annotate@metadata$mapping,
#                  "user provided ids")
# })
