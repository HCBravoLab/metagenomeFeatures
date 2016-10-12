## for generating the demoMgF
library(metagenomeFeatures)

## required MgDb with mock community ids
mockMgDb <- get_mockMgDb()

## generating mgFeatures object
data(mock_query_df)
mock_mgF <- annotateFeatures(mockMgDb, mock_query_df)

## saving as rdata
save(mock_mgF, file = "../../data/mock_mgF.RData")
