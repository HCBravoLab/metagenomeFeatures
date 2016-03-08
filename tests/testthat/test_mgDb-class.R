library(metagenomeFeatures)

## test generate MgDb object
db_seq <- Biostrings::readDNAStringSet("../testSeq.fasta.gz")
metadata <- list(ACCESSION_DATE = "3/31/2015",
                 URL = "https://greengenes.microbio.me",
                 DB_TYPE_NAME = "GreenGenes",
                 DB_TYPE_VALUE = "MgDb",
                 DB_SCHEMA_VERSION = "1.0")


taxdb_file <- "../testTaxa.sqlite3"

testMgDb <- new("MgDb", seq = db_seq,
                taxa_file = taxdb_file,
                metadata = metadata)

context("mgDb-class")

## uses expect_equal_to_reference, caches the results from the first time it is run and compares to future runs
test_that("MgDb-class",{
    expect_is(testMgDb, "MgDb")
})

## Not actually testing the show method
# test_that("MgDb-class show", {
#     expect_equal_to_reference(show(testMgDb),
#                               file = "cache/MgDb_test_show.rds")
# })

test_that("MgDb-class metadata", {
    expect_equal(testMgDb$metadata$ACCESSION_DATE,"3/31/2015")
    expect_equal(testMgDb$metadata$URL, "https://greengenes.microbio.me")
    expect_equal(testMgDb$metadata$DB_TYPE_NAME, "GreenGenes")
    expect_equal(testMgDb$metadata$DB_TYPE_VALUE, "MgDb")
    expect_equal(testMgDb$metadata$DB_SCHEMA_VERSION, "1.0")
})

test_that("MgDb-class seq",{
    expect_is(testMgDb$seq, "DNAStringSet")
    expect_equal(testMgDb$seq, db_seq)
})

test_that("MgDb-class taxa",{
    expect_is(testMgDb$taxa, "tbl_sqlite")
})

taxa_levels = c("Keys","Kingdom","Phylum","Class","Order","Family","Genus","Species")
test_that("MgDb-class taxa_keytypes", {
    expect_equal_to_reference(taxa_keytypes(testMgDb),
                              file = "cache/MgDb_test_taxa_keytypes.rds")
    expect_identical(length(taxa_keytypes(testMgDb)), length(taxa_levels))
    expect_equal(taxa_keytypes(testMgDb)[1], taxa_levels[1])
    expect_equal(taxa_keytypes(testMgDb)[2], taxa_levels[2])
    expect_equal(taxa_keytypes(testMgDb)[3], taxa_levels[3])
    expect_equal(taxa_keytypes(testMgDb)[4], taxa_levels[4])
    expect_equal(taxa_keytypes(testMgDb)[5], taxa_levels[5])
    expect_equal(taxa_keytypes(testMgDb)[6], taxa_levels[6])
    expect_equal(taxa_keytypes(testMgDb)[7], taxa_levels[7])
    expect_equal(taxa_keytypes(testMgDb)[8], taxa_levels[8])
})

test_that("MgDb-class taxa_columns", {
    expect_equal_to_reference(taxa_columns(testMgDb),
                              file = "cache/MgDb_test_taxa_columns.rds")
})

test_that("MgDb-class taxa_keytypes and taxa_columns are identical", {
    expect_identical(taxa_columns(testMgDb), taxa_keytypes(testMgDb))
})

test_that("MgDb-class taxa_keys at different taxonomic levels", {
    expect_error(taxa_keys(testMgDb, keytype = c("not a type")))
    expect_error(taxa_keys(testMgDb, keytype = c("not a type","neither is this")))
    expect_error(taxa_keys(testMgDb, keytype = c("not a type","Kingom")))
    expect_equal(taxa_keys(testMgDb, keytype = c()), testMgDb$taxa)
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Kingdom")),
                              file = "cache/MgDb_test_taxa_keys_Kingdom.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Phylum")),
                              file = "cache/MgDb_test_taxa_keys_Phylum.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Class")),
                              file = "cache/MgDb_test_taxa_keys_Class.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Order")),
                              file = "cache/MgDb_test_taxa_keys_Order.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Family")),
                              file = "cache/MgDb_test_taxa_keys_Family.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Genus")),
                              file = "cache/MgDb_test_taxa_keys_Genus.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Species")),
                              file = "cache/MgDb_test_taxa_keys_Species.rds")
    expect_equal_to_reference(taxa_keys(testMgDb, keytype = c("Kingdom","Species")),
                              file = "cache/MgDb_test_taxa_keys_Kingdom_Species.rds")

})


## Select Methods
test_that("MgDb-class select",{
    expect_error(select(testMgDb, type = "not a type"))
    expect_error(select(testMgDb, type = "both", keytype = "Keys"))
    expect_error(select(testMgDb, type = "both", keys = "Streptomyces"))
})

test_select_taxa <- select(testMgDb, type = "taxa",
                           keys = c("Streptomyces", "Prevotella"),
                           keytype = "Genus")

test_that("MgDb-class select taxa",{
    expect_equal_to_reference(test_select_taxa,
                              file = "cache/MgDb_test_select_taxa.rds")
    expect_is(test_select_taxa,"tbl_df")
    test_select_taxa <- select(testMgDb, type = "taxa",
                               keys = c("Streptomyces"),
                               keytype = "Genus")
    expect_equal(test_select_taxa$Keys, "632432")
    expect_equal(test_select_taxa$Kingdom,"k__Bacteria")
    expect_equal(test_select_taxa$Phylum,"p__Actinobacteria")
    expect_equal(test_select_taxa$Class,"c__Actinobacteria")
    expect_equal(test_select_taxa$Order,"o__Actinomycetales")
    expect_equal(test_select_taxa$Family,"f__Streptomycetaceae")
    expect_equal(test_select_taxa$Genus,"g__Streptomyces")
    expect_equal(test_select_taxa$Species,"s__")
})

test_select_seq <- select(testMgDb, type = "seq",
                          keys = c("Streptomyces", "Prevotella"),
                          keytype = "Genus")

test_that("MgDb-class select seq", {
    expect_equal_to_reference(
        test_select_seq,
        file = "cache/MgDb_test_select_seq.rds")
    expect_is(test_select_seq, "DNAStringSet")
})

test_select_both <- select(testMgDb, type = "both",
                           keys = c("Streptomyces", "Prevotella"),
                           keytype = "Genus")

test_that("MgDb-class select both",{
    expect_equal_to_reference(
        test_select_both,
        file = "cache/MgDb_test_select_both.rds")
    expect_is(test_select_both, "list")
    expect_is(test_select_both$taxa, "tbl_df")
    expect_is(test_select_both$seq, "DNAStringSet")
    expect_equal(test_select_both$taxa, test_select_taxa)
    expect_equal(test_select_both$seq, test_select_seq)
})

test_db_keys <- c("4324716", "246960", "222675", "156874", "4383832")
test_query_df <- data.frame(Keys = test_db_keys)
test_query_seq = ShortRead::sread(mgQuery[1:5])
test_annotate <- annotate(testMgDb, query_seq = test_query_seq,
                          query_df = test_query_df)
test_annotate_no_query_seq <-  annotate(testMgDb, db_keys = test_db_keys)
test_that("MgDb-class annotate check arguments-query_seq only", {
    expect_error(annotate(testMgDb, query_seq = test_query_seq))
})

test_that("MgDb-class annotate check arguments-query_df no Keys Column", {
    expect_error(annotate(testMgDb, query_df = data.frame(NotKeys = test_db_keys)))
})

test_that("MgDb-class annotate check arguments-db_keys only", {
    expect_message(annotate(testMgDb, db_keys = test_db_keys))
})
# ## Add tests for query_df
# ##  make sure able to filter
# ##  make sure added to dataframe
#
# test_that("MgDb-class annotate regression test",{
#     expect_equal_to_reference(
#         test_annotate,
#         file = "cache/MgDb_test_annotate.rds")
# })

## should this be moved to validity check?
test_that("MgDb-class annotate test class types",{
    expect_is(test_annotate, "metagenomeAnnotation")
    expect_is(test_annotate@metadata, "list")
    expect_is(test_annotate@experimentSeqData, "DNAStringSet")
})


test_that("MgDb-class annotate test selected data size",{
    expect_equivalent(nrow(test_annotate),
                      length(test_annotate@experimentSeqData))
    expect_equivalent(length(test_annotate_no_query_seq@experimentSeqData), 0)
    expect_equivalent(nrow(test_annotate_no_query_seq), 5)
})


test_that("MgDb-class annotate metadata", {
    expect_equal(test_annotate@metadata$ACCESSION_DATE,
                 testMgDb$metadata$ACCESSION_DATE)
    expect_equal(test_annotate@metadata$URL,
                 testMgDb$metadata$URL)
    expect_equal(test_annotate@metadata$DB_TYPE_NAME,
                 testMgDb$metadata$DB_TYPE_NAME)
    expect_equal(test_annotate@metadata$DB_TYPE_VALUE,
                 testMgDb$metadata$DB_TYPE_VALUE)
    expect_equal(test_annotate@metadata$DB_SCHEMA_VERSION,
                 testMgDb$metadata$DB_SCHEMA_VERSION)
    expect_equal(test_annotate@metadata$mapping,
                 "user provided ids")
})


library(metagenomeSeq)

count_dat <- data.frame(sam1 = c(1:10))
OTUcenters <- c("813058","4334144","517968","1113159",
                "3058155","4378325","60159","2569970",
                "4474227","48617")

rownames(count_dat) <- OTUcenters

testMRobj <- newMRexperiment(counts = count_dat)
testMRobj <- annotateMRexp(mgdb = testMgDb, MRobj = testMRobj)

test_that("MgDb-class annotateMRexp",{
    expect_equal_to_reference(annotateMRexp(mgdb = testMgDb, MRobj = testMRobj),
                              file = "cache/MgDb_test_annotateMRexp.rds")
})
