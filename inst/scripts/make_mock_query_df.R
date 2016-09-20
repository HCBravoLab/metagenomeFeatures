#Generating data files for vignette and examples
library(metagenomeSeq)
library(biomformat)

## Bug in load_biom - metagenomeSeq converting hdf5 to json biom file format
mock_biom <- biomformat::read_hdf5_biom("../extdata/6692_analysis_dt-16S_r-1_c-3.biom")
write_biom(mock_biom, "6692_analysis_dt_16S_r-1_c-3_biomformatv1.biom")

#loading biom file as MRexperiment object
mock_mrexp <- load_biom("6692_analysis_dt_16S_r-1_c-3_biomformatv1.biom")

#dataframe with count table and OTU ids for generating metagenomeAnnotation
mock_otu_counts <- MRcounts(mock_mrexp)
mock_query_df <- data.frame(Keys = rownames(mock_otu_counts),OTU = 1:nrow(mock_otu_counts))

save(mock_query_df, file = "../../data/mock_query_df.RData")

# saving count table data for vignettes
exportMat(obj = mock_mrexp, log = FALSE, norm = FALSE, sep = ",",file = "../extdata/mock_counts.csv")

# saving pheno data for vignettes
write.csv(pData(mock_mrexp), file = "../extdata/mock_sample_data.csv")

