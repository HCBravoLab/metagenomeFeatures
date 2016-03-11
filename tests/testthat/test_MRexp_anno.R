# ## annotating MRexp
# library(metagenomeSeq)
#
# count_dat <- data.frame(sam1 = c(1:5))
#
# OTUcenters <- as.character(1:5)
#
# rownames(count_dat) <- OTUcenters
#
# testMRobj <- newMRexperiment(counts = count_dat)
# testMRobj <- annotateMRexp(mgdb = testMgDb, MRobj = testMRobj)
#
# #test_that("MgDb-class annotateMRexp",{
# #    expect_equal_to_reference(annotateMRexp(mgdb = testMgDb, MRobj = testMRobj),
# #                              file = "cache/MgDb_test_annotateMRexp.rds")
# #})
