library(metagenomeFeatures)
library(dplyr)


context("Testing annotateFeatures")

OTU <- c(4,4,4,3,2)
Keys_not <- c(1,1,1,1,1)
Keys <- c(1,2,3,4,5)

incomplete_query_df <- data.frame(OTU)
wrong_query_df <- data.frame(Keys_not, OTU)
query_df <- data.frame(Keys, OTU)


# check parameters

test_that("annotateFeatures parameter check", {
	expect_error(annotateFeatures(testMgDb))
	expect_error(annotateFeatures(testMgDb, query = NULL))
	expect_error(annotateFeatures(testMgDb, incomplete_query_df))
	expect_error(annotateFeatures(testMgDb, wrong_query_df))
})


# check resulting mgFeatures object result

test_that("mgFeatures object", {

	features_obj <- annotateFeatures(testMgDb, query_df)

	# check type breakdown of mgFeatures object
	expect_is(features_obj@metadata, "list")
	expect_is(features_obj@refDbSeq, "DNAStringSet")
	expect_is(features_obj@refDbTree, "phylo")
	expect_is(features_obj@varMetadata, "data.frame")
	expect_is(features_obj@data, "data.frame")
	expect_is(features_obj@dimLabels, "character")

	expect_is(mgF_seq(features_obj), "DNAStringSet")
	expect_is(mgF_taxa(features_obj), "data.frame")
	expect_is(mgF_tree(features_obj), "phylo")
	expect_is(mgF_meta(features_obj), "list")

})
