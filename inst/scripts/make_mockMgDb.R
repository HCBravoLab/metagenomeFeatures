## Database with a random representative sequence for each genus

library(dplyr)
library(metagenomeFeatures)
<<<<<<< HEAD
#https://github.com/nate-d-olson/greengenes13.8_99MgDb
=======
>>>>>>> a583485a53d90aa439a78b7bce0e232981ba1e83
library(greengenes13.8OTU99MgDb)
library(Biostrings)
library(ape)
data(mock_query_df)


set.seed(1) # setting seed so that the sampled genus are the same if script is rerun
genus_df <- mgdb_taxa(gg13.8.99MgDb)  %>% collect(n = Inf) %>%
    group_by(Kingdom, Phylum, Class, Order, Family, Genus) %>% sample_n(1)

## Saving taxa data
write.csv(genus_df, "../extdata/gg_rep_genus_df.csv")
## Saving keys to file
genus_keys <- genus_df$Keys
save(genus_keys, file = "../extdata/genus_keys.rdata")

## generating dataset with both ID keys for mock dataset and single
## representative key for each genus
combined_keys <- c(genus_df$Keys, mock_query_df$Keys)


# keys = combined keys
<<<<<<< HEAD
gg_data <- mgDb_select(gg13.8.99MgDb, type = c("seq", "taxa","tree"),
                       keys = combined_keys, keytype = "Keys")


# save as fasta.gz file - saving as rdata - includes environment info, resulting in a larger than necessary file
writeXStringSet(gg_data$seq,
                filepath = "../extdata/mockSeq.fasta.gz",
                compress = TRUE)
=======
gg_data <- mgDb_select(gg13.8.99MgDb, type = c("seq", "taxa","tree"), keys = combined_keys, keytype = "Keys")


# save as fasta.gz file - saving as rdata - includes environment info, resulting in a larger than necessary file
writeXStringSet(gg_data$seq, filepath = "../extdata/mockSeq.fasta.gz", compress = TRUE)
>>>>>>> a583485a53d90aa439a78b7bce0e232981ba1e83

# save taxa info as sqlite db
src_sqlite("../extdata/mockTaxa.sqlite", create = TRUE) %>%
    copy_to(gg_data$taxa, name = "taxa", temporary = FALSE)

saveRDS(gg_data$tree, file = "../extdata/mockTree.rds")
