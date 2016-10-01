## Setting phylo and phyloOrNULL class for MGDb and mgFeatures

setOldClass("phylo")

## Borrowed from https://github.com/joey711/phyloseq/blob/master/R/allClasses.R
# Use setClassUnion to define the unholy NULL-data union as a virtual class.
# This is a way of dealing with the expected scenarios in which one or more of
# the component data classes is not available, in which case NULL will be used
# instead.
#' @keywords internal
setClassUnion("phyloOrNULL", c("phylo", "NULL"))
