## Generate modified metadata file for retrieve-feature-data vignette
library(tidyverse)
sample_dat <- read_tsv("../2301_mapping_file.txt") %>%
     dplyr::rename(SampleID = `#SampleID`) %>%
     select(SampleID, carb_nitro_ratio, ph, tot_nitro, tot_org_carb)

write_tsv(sample_dat, "../extdata/229_sample_data.tsv")
