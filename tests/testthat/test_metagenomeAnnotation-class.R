library(metagenomeFeatures)
library(ShortRead)
testMgAnno <- readRDS("../testMgAnnoDF.rds")

context("metagenomeAnnotation-class")
test_that("metagenomeAnnotation method split_by", {
    expect_equal_to_reference(
        split_by(testMgAnno, "Kingdom"),
        file = "cache/metagenomeAnnotation_split_by_Kingdon.rds")
    expect_equal_to_reference(
        split_by(testMgAnno,"Phylum"),
        file = "cache/metagenomeAnnotation_split_by_Phylum.rds")
    expect_equal_to_reference(
        split_by(testMgAnno,"Class"),
        file = "cache/metagenomeAnnotation_split_by_Class.rds")
    expect_equal_to_reference(
        split_by(testMgAnno,"Order"),
        file = "cache/metagenomeAnnotation_split_by_Order.rds")
    expect_equal_to_reference(
        split_by(testMgAnno,"Family"),
        file = "cache/metagenomeAnnotation_split_by_Family.rds")
    expect_equal_to_reference(
        split_by(testMgAnno,"Genus"),
        file = "cache/metagenomeAnnotation_split_by_Genus.rds")
    expect_equal_to_reference(
        split_by(testMgAnno,"Species"),
        file = "cache/metagenomeAnnotation_split_by_Species.rds")
})
