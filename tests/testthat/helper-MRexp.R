library(metagenomeSeq)
library(metagenomeFeatures)


## Test Count ------------------------------------------------------------------
make_test_count <- function(n, single_sample){
    test_count <- data.frame(sam1 = c(1:n), row.names = paste0("OTU_",1:n))
    if(!single_sample) test_count$sam2 <- c(1:n)^2
    test_count
}

## Test mgF --------------------------------------------------------------------
make_test_taxa <- function(n, taxa_levels){
    n_taxa_levels <- length(taxa_levels)
    OTU <- paste0("OTU_",1:n)
    tax_ids <- paste0("tax_",c(0:(n*(n_taxa_levels-1)-1)))

    test_taxa <- matrix(c(OTU, tax_ids), ncol = n_taxa_levels) %>%
        as.data.frame(row.names = OTU, stringsAsFactors = FALSE)

    colnames(test_taxa) <-c(taxa_levels)
    test_taxa
}

make_test_mgF <- function(n, taxa_levels, test_seq, test_tree, all_unique){

    test_taxa <- make_test_taxa(n, taxa_levels)

    ## add replicate taxa for testing aggregateTaxa
    if(!all_unique){
        n_lvl <- length(taxa_levels)
        test_taxa[2,2:n_lvl] <- test_taxa[1,2:n_lvl ]
        test_taxa[4,2:(n_lvl -1)] <- test_taxa[3,2:(n_lvl - 1)]
        test_taxa[6,2:(n_lvl -2)] <- test_taxa[5,2:(n_lvl - 2)]
    }

    new("mgFeatures",
        data = test_taxa,
        metadata = list(),
        refDbSeq = test_seq,
        refDbTree = test_tree)
}

make_test_MRexp <- function(n, taxa_levels, test_seq, test_tree, single_sample = FALSE, all_unique = FALSE){
    test_count <- make_test_count(n,single_sample)

    test_mgF <- make_test_mgF(n, taxa_levels, test_seq, test_tree, all_unique)

    metagenomeSeq::newMRexperiment(count = test_count, featureData = test_mgF)
}



test_seq <- readRDS("../test_seq.rds")
test_tree <- readRDS("../test_tree.rds") %>% as(Class = "phylo")

test_MRexp <-make_test_MRexp(n = 10,
                            taxa_levels = c("OTU","Kingdom",
                                            "Phylum","Class","Order",
                                            "Family","Genus","Species"),
                            test_seq, test_tree)

test_MRexp1 <-make_test_MRexp(n = 10,
                             taxa_levels = c("OTU","Kingdom",
                                             "Phylum","Class","Order",
                                             "Family","Genus","Species"),
                             test_seq, test_tree, single_sample = TRUE)


test_MRexp_all_unique <- make_test_MRexp(n = 10,
                                       taxa_levels = c("OTU","Kingdom",
                                                       "Phylum","Class","Order",
                                                       "Family","Genus","Species"),
                                       test_seq, test_tree, all_unique = TRUE)
