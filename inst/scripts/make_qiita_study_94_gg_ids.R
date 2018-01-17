## Generate RDS with features for closed reference clustering example
## URL for QIITA - https://qiita.ucsd.edu/study/description/94
## The biom file and mapping data were downloaded from the database and biom file was copied into the package extdat directory
library(purrr)
library(biomformat)

## Read BIOM File
biom_dat <- read_hdf5_biom("../extdata/229_otu_table.biom")


## Extract Greengenes IDS for cluster centers
row_dat <- biom_dat$rows
gg_ids <- row_dat %>% map_chr("id")

## SAVE as RDS
saveRDS(gg_ids, "../extdata/qiita_study_94_gg_ids.RDS")
