#script to generate MRexperiment object
library(metagenomeSeq)
library(biomformat)
#loading biom file as MRexperiment object
#MRexperiment objects contain taxonomic annotations relative to associated OTUs
mock_mrexp <- load_biom("../extdata/6692_analysis_dt-16S_r-1_c-3.biom")

#set otuCounts to OTU frequency per sample
mock_otu_counts <- MRcounts(mock_mrexp)

#fData used as test to verify annotations

#head(fData(mock_mrexp))

#queryDF (key: rownames for each OTU, value: OTU freq)

#
# head(msd16s_query_df)
#

#set up dataframe of key ids from database with associated OTU ids
mock_query_df <- data.frame(Keys = rownames(mock_otu_counts),OTU = 1:nrow(mock_otu_counts))

#save mock_query_df as object file in appropriate folder
#saveRDS(mock_query_df, "../data/mock_query_df.rdata")

save(mock_query_df, file = "../../data/mock_query_df.RData")


