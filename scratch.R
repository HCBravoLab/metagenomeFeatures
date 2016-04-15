#library(metagenomeFeatures)
library(ShortRead)
library("Biostrings")
## test generate MgDb object
db_seq <- readDNAStringSet("~/greengenes13.5MgDb/data/gg_13_5.fasta.gz")
metadata <- list(ACCESSION_DATE = "3/31/2015",
                 URL = "https://greengenes.microbio.me",
                 DB_TYPE_NAME = "GreenGenes",
                 DB_TYPE_VALUE = "MgDb",
                 DB_SCHEMA_VERSION = "1.0")

testMgDb <- new("MgDb",seq = db_seq, taxa = "~/greengenes13.5MgDb/data/gg_13_5.sqlite3", metadata = metadata)

testMgDb

## testing taxa_keytypes
taxa_keytypes(testMgDb)

taxa_columns(testMgDb)

head(taxa_keys(testMgDb, keytype = c("Kingdom")))


## Select Methods
### Used to retrieve db entries for a specified taxanomic group or id list
select(testMgDb, type = "taxa",
                keys = c("Vibrio", "Salmonella"),
                keytype = "Genus")

select(testMgDb, type = "seq",
                keys = c("Vibrio", "Salmonella"),
                keytype = "Genus")

select(testMgDb, type = "both",
                keys = c("Vibrio", "Salmonella"),
                keytype = "Genus")

## Creating an metagenomeAnnotation class object
### example query data - not really matching

query_subset <- mgQuery[1:100]

testMgAnnoDF <- testMgDb$annotate(query = query_subset, mapping = "arbitrary")


testSplit_MgAnno <- split_by(testMgAnnoDF, "Class")

library(Biostrings)
## test generate MgDb object
db_seq <- readDNAStringSet("tests/testSeq.fasta.gz")
metadata <- list(ACCESSION_DATE = "3/31/2015",
                 URL = "https://greengenes.microbio.me",
                 DB_TYPE_NAME = "GreenGenes",
                 DB_TYPE_VALUE = "MgDb",
                 DB_SCHEMA_VERSION = "1.0")

testMgDb <- new("MgDb", seq = db_seq,
                taxa = "tests/testTaxa.sqlite3",
                metadata = metadata)


select(testMgDb, type = "taxa",
       keys = c("Streptomyces", "Prevotella"),
       keytype = "Genus")


################################################################################
#============================================ annotating MRexperiment with mgDb

annotateTaxa <- function(MRobj, mgdb){
    ## subset mgdb using MRobj - featureNames from MRobj$assay
    featureData(MRobj) <- annotate(mgdb,db_keys = featureNames(MRobj))
}

library(metagenomeFeatures)
library(metagenomeSeq)
#library(greengenes13.5MgDb)
library(msd16s)

metadata = list(URL = "https://greengenes.microbio.me",
                DB_TYPE_NAME = "GreenGenes",
                DB_VERSION = "gg_13_5",
                ACCESSION_DATE = "July 20, 2015")
### issues with greengenes13.5MgDb building MgDb object
mgdb <- new("MgDb",seq = readRDS("../greengenes13.5MgDb/inst/extdata/gg_13_5_seq.rds"),
              taxa_file = "../greengenes13.5MgDb/inst/extdata/gg_13_5.sqlite3",
              metadata = metadata)
MRobj <- newMRexperiment(counts = msd16s@assayData$counts,phenoData = msd16s@phenoData)

## TODO - work out tests
## 1. missing FeatureNames missing - need to check db
## 2. missing some feature names - return MRobj without annotating
annotateTaxa(MRobj = MRobj,mgdb = mgdb)



