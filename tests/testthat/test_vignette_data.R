context("vignette_data")

## checking vignette data loads
test_that("vignette_assay_data",{
    expect_is(vignette_assay_data(),"list")
})

test_that("vignette_pheno_data",{
    expect_is(vignette_pheno_data(),"AnnotatedDataFrame")
})
