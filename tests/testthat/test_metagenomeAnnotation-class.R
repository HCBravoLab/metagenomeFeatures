# library(metagenomeFeatures)
# testMgAnno <- readRDS("../testMgAnno.rds")
#
# context("metagenomeAnnotation-class")
# split_kingdom <- split_by(testMgAnno, "Kingdom")
# test_that("metagenomeAnnotation method split_by",{
#     expect_is(split_kingdom, "list")
#     expect_is(split_kingdom$k__Bacteria, "metagenomeAnnotation")
#     expect_is(split_kingdom$k__Bacteria@metadata, "list")
#     expect_is(split_kingdom$k__Bacteria@experimentSeqData, "DNAStringSet")
# })

# test_that("metagenomeAnnotation method split_by taxa levels", {
#     expect_equal_to_reference(
#         split_kingdom,
#         file = "cache/metagenomeAnnotation_split_by_Kingdom.rds")
#     expect_equal_to_reference(
#         split_by(testMgAnno,"Phylum"),
#         file = "cache/metagenomeAnnotation_split_by_Phylum.rds")
#     expect_equal_to_reference(
#         split_by(testMgAnno,"Class"),
#         file = "cache/metagenomeAnnotation_split_by_Class.rds")
#     expect_equal_to_reference(
#         split_by(testMgAnno,"Order"),
#         file = "cache/metagenomeAnnotation_split_by_Order.rds")
#     expect_equal_to_reference(
#         split_by(testMgAnno,"Family"),
#         file = "cache/metagenomeAnnotation_split_by_Family.rds")
#     expect_equal_to_reference(
#         split_by(testMgAnno,"Genus"),
#         file = "cache/metagenomeAnnotation_split_by_Genus.rds")
#     expect_equal_to_reference(
#         split_by(testMgAnno,"Species"),
#         file = "cache/metagenomeAnnotation_split_by_Species.rds")
# })
