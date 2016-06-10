# get keys 

## Database with a random representative sequence for each genus

library(dplyr)
library(metagenomeFeatures)
library(greengenes13.5MgDb)
library(Biostrings)
library(ape)
data(msd16s_query_df)

ggtaxa <- gg13.5MgDb$taxa

set.seed(1) # setting seed so that the sampled genus are the same if script is rerun
genus_df <- ggtaxa  %>% collect() %>% group_by(Kingdom, Phylum, Class, Order, Family, Genus) %>% sample_n(1)
write.csv(genus_df,"../extdata/gg_genus_df.csv")

## Saving keys to file
genus_keys <- genus_df$Keys
save(genus_keys, file = "../extdata/genus_keys.RData")

# combine in query_df keys to genus_keys to
mock_keys <- msd16s_query_df$Keys

# mock_keys (integer), genus_keys (character)
# combining turns everything into character
combined_keys <- c(genus_keys, mock_keys)

# keys = combined keys
gg_data <- mgDb_select(gg13.5MgDb, type = c("seq", "taxa"), keys = combined_keys, keytype = "Keys")


# save as RData file
seq_data <- gg_data$seq

#seq_db <- src_sqlite("../extdata/mockSeq.sqlite", create = TRUE)

saveRDS(seq_data,file = "../extdata/mockSeq.rds")


#seq_sqlite <- copy_to(seq_db, seq_data, temporary = FALSE)

# turn this into sqlite and save
taxa_data <- gg_data$taxa
taxa_db <- src_sqlite("../extdata/mockTaxa.sqlite", create = TRUE)

copy_to(taxa_db, taxa_data, name = "taxa", temporary = FALSE)
