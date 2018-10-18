## Setting phylo and phyloOrNULL class for MGDb and mgFeatures

setOldClass("DNAStringSetOrNull")

## Borrowed from https://github.com/joey711/phyloseq/blob/master/R/allClasses.R
# Use setClassUnion to define the unholy NULL-data union as a virtual class.
# This is a way of dealing with the expected scenarios in which one or more of
# the component data classes is not available, in which case NULL will be used
# instead.
#' @keywords internal
#' @importClassesFrom Biostrings DNAStringSet
#' @importClassesFrom Biostrings QualityScaledDNAStringSet
setClassUnion("DNAStringSetOrNull", c("DNAStringSet", "NULL"))
