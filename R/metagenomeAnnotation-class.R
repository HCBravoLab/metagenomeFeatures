## =============================================================================
## metagenomeAnnotation object
## the code below is modeled after the TxDb class in GenomicFeatures package
## this object is generated from metagenomeDb class and contains database object
## along with metadata about the experiment and annotation data source
## -----------------------------------------------------------------------------


## genomicFeatures uses garbage collection to clean up the environment/ memory
## prior to setRefClass() would this be appropriate here as well?
# gc()


## metagenomeAnnotation class definition ----------------------------------------------
.metagenomeAnnotation <- setRefClass("metagenomeAnnotation", contains="ShortRead",
                              fields=list(taxa="ANY", #use of any as dplyr tbl is not a recognized class
                                          seq="ShortRead",
                                          features="list"
                              ))

## MetagenomeExp Validity ------------------------------------------------------
## Note the object may not necessarily have both ShortRead and taxa data.frame

setValidity("metagenomeAnnotation", function(object) {
    msg <- NULL
    if(!("seq" %in% ls(object)) || !is(object@seq, "ShortRead"))
        msg <- paste(msg, "'seq' slot must contain a ShortRead object with sequence data", sep = "\n")
    if(!("taxa" %in% ls(object)) || !is(object@taxa, "tbl_sqlite"))
        msg <- paste(msg, "'taxa' slot must contain a tbl_sqlite object with taxonomy data", sep = "\n")
    if(!("metadata" %in% ls(object)) || !is(object@metadata, "list"))
        msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
    if (is.null(msg)) TRUE else msg
})

## metagenomeAnnotation methods -------------------------------------------------------
