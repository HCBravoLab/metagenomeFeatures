## hidden functions for vignette data
vignette_assay_data <- function(){
    system.file("extdata", "mock_counts.csv",
                package = "metagenomeFeatures") %>%
        metagenomeSeq::loadMeta(sep = ",")
}


vignette_pheno_data <- function(){
    system.file("extdata", "mock_sample_data.csv",
                package = "metagenomeFeatures")  %>%
        read.csv(row.names = 1, stringsAsFactors = FALSE) %>%
        Biobase::AnnotatedDataFrame()
}

vignette_mock_MgDb <- function(){
    ## note same as demoMgDb
    get_demoMgDb()
}
