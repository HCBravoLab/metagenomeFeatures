## preparing data to demonstrate annotating MRExperiment
## required inputs
library(msd16s)
data("msd16s")
library(dplyr)
library(Biostrings)
library(greengenes13.5MgDb)
library(readr)
library(metagenomeFeatures)
library(metagenomeSeq)
# ## 1. count data
# dataDirectory <- system.file("extdata",package = "metagenomeSeq")
# count_data <- read.csv(file.path(dataDirectory, "CHK_NAME.otus.count.csv"), sep = "\t")
#
# ## 2. pheno data
# clin_data <- read.csv(file.path(dataDirectory, "CHK_clinical.csv"), sep = "\t")
#
# ## 3. seq data
# seq_data <- DNAStringSet(fData(msd16s)$clusterCenter)
# otu_ids = rownames(fData(msd16s))
# names(seq_data) <- rownames(fData(msd16s))
#
# ## 3. taxa id data
# data(msd16s_query_df)
#
# ## 4. demoMgDb
# msd16s_MgDb_data <- select(gg13.5MgDb,type = "both", keys = msd16s_query_df$Keys, keytype = "Keys")
# msd16s_MgDb_data
#
# msd16s_db <- src_sqlite("msd16s_MgDb.sqlite",create = T)
# copy_to( msd16s_db, msd16s_MgDb_data$taxa, name = "taxa",temporary = F)
#
# writeXStringSet(msd16s_MgDb_data$seq, "msd16s_seq.fasta.gz", compress = T)
#
# ## modify get_demoMgDb - add default parameter for demo, if not msd16s
    metadata <- list(ACCESSION_DATE = "3/31/2015",
                     URL = "https://greengenes.microbio.me",
                     DB_TYPE_NAME = "GreenGenes-MgDb-msd16s",
                     DB_TYPE_VALUE = "MgDb",
                     DB_SCHEMA_VERSION = "1.0")
    msd16s_MgDb <- new("MgDb",
                    seq = readDNAStringSet("inst/extdata/msd16s_MgDb_seq.fasta.gz"),
                    taxa = "inst/extdata/msd16s_MgDb_taxa.sqlite",
                    metadata = metadata)
#
#
# ## Creating msd16s MRExperiment
# dataDirectory <- system.file("extdata", package="metagenomeSeq")
# lung = load_meta(file.path(dataDirectory,"CHK_NAME.otus.count.csv"))
#
# clin = load_phenoData(file.path(dataDirectory,"CHK_clinical.csv"),tran=TRUE)
# ord = match(colnames(lung$counts),rownames(clin))
# clin = clin[ord,]
# phenotypeData = AnnotatedDataFrame(clin)
#
#
# msd16s_feature_data <- annotate(gg13.5MgDb,query_df = arrange(msd16s_query_df, as.numeric(as.character(OTU)))[1:1000,])
# obj = newMRexperiment(lung$counts,phenoData=phenotypeData,featureData = msd16s_feature_data)
#
#
# taxa = read.delim(file.path(dataDirectory,"CHK_otus.taxonomy.csv"),stringsAsFactors=FALSE)
# obj = newMRexperiment(lung$counts,phenoData=phenotypeData,featureData = AnnotatedDataFrame(taxa))
#
# # message/ warning for when less then query_df rownumber != select rows
#
#
# tax_assign <- read_tsv("~/Google Drive/UMD-Projects/16S-UMD/msd16s_gg_13_5_ids/mothur_assigned_taxonomy/msdSeq_tax_assignments.txt",col_names = FALSE)
#
# msdTax <- data.frame(Keys = as.character(tax_assign$X1), OTU = rownames(fData(msd16s)))
#
#
# msdMgAnno <- annotate(gg13.5MgDb, query_df = msdTax)
#
# ## take 2 counts
# MRcounts(msd16s) %>% as.data.frame() %>% mutate(OTU = row.names(.)) %>% filter(OTU %in% msd16s_query_df$OTU) %>% select(-OTU) %>% write.csv("msd16s_counts.csv")
# count_data <- load_meta("msd16s_counts.csv",sep = ",")
#
# ## pheno data
# pData(msd16s) %>% write.csv("msd16s_sample_data.csv", row.names = row.names(.))
# pheno_data <- read.csv("msd16s_sample_data.csv", row.names = 1, stringsAsFactors = F) %>% AnnotatedDataFrame()
#
# msd16s_feature_data <- annotate(gg13.5MgDb, query_df = msd16s_query_df)
#
# msd16s_feature_data
#
# ## need to modify so that the same entries are in all datasets
#
# obj = newMRexperiment(count_data$counts,phenoData=pheno_data, featureData = msd16s_feature_data)
#
#
# # Error in validObject(.Object) :
# #     invalid class “MRexperiment” object: 1: feature numbers differ between assayData and featureData
# # invalid class “MRexperiment” object: 2: featureNames differ between assayData and featureData
#
# ## filtering count data
# msd16s_feature_data
# anno_rows <- rownames(msd16s_feature_data) %>% as.numeric()
#
# count_df <- MRcounts(msd16s) %>% as.data.frame() %>% mutate(OTU = row.names(.)) %>% filter(OTU %in% anno_rows)
# row.names(count_df) <- count_df$OTU
# count_df$OTU <- NULL
# write.csv(count_df, "msd16s_counts.csv")

# count data
count_data <- load_meta("msd16s_counts.csv",sep = ",")

# pheno data
pheno_data <- read.csv("msd16s_sample_data.csv", row.names = 1, stringsAsFactors = F) %>% AnnotatedDataFrame()


# wonky methond to assing featureNames
data("msd16s_query_df")
# msd16s_feature_data <- annotate(gg13.5MgDb, query_df = msd16s_query_df)

msd16s_feature_data <- annotate(gg13.5MgDb, query_df = msd16s_query_df)
featureNames(msd16s_feature_data) <- as.numeric(as.character(msd16s_feature_data$OTU))
ord <-  as.numeric(as.character(msd16s_feature_data$OTU)) %>% sort()
msd16s_feature_data <- msd16s_feature_data[order(as.numeric(as.character(msd16s_feature_data$OTU))),]

obj = newMRexperiment(count_data$counts,phenoData=pheno_data, featureData = msd16s_feature_data)

