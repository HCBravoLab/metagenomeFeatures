## =============================================================================
## metagenomeAnnotation object
## the code below is modeled after the TxDb class in GenomicFeatures package
## this object is generated from metagenomeDb class and contains database object
## along with metadata about the experiment and annotation data source
## -----------------------------------------------------------------------------


## metagenomeAnnotation Class

## Annotated Dataframe
##  User provided ids joined with ref db
##  annotation metadata - ref db package, mapping method, user defined features, e.g. annotate function parameters
##  feature data - cluster sequences, cluster IDs
setClass("metagenomeAnnotation",
         representation = list(mgAnnotatedDF = "AnnotatedDataFrame",
                               metadata = "list",
                               featureData = "DNAStringSet"),
         contains = c("AnnotatedDataFrame","DNAStringSet")
)

## Warning message when defining class
# Warning messages:
#     1: class "AnnotatedDataFrame" is defined (with package slot ‘Biobase’) but no metadata object found to revise subclass information---not exported?  Making a copy in package ‘.GlobalEnv’
# 2: class "Versioned" is defined (with package slot ‘Biobase’) but no metadata object found to revise subclass information---not exported?  Making a copy in package ‘.GlobalEnv’


## defining non-specified slots - empty for now
setMethod("prototype", "metagenomeAnnotation",
          function(.Object,...){
              mgAnnotatedDF <- new("AnnotatedDataFrame")
              metadata <- list()
              featureData <- new("DNAStringSet")
          }
)

## for use in creating a new object
setMethod("initialize","metagenomeAnnotation",
          function(.Object,refDF,metadata, feature_data, ...){
              ## mgAnnoatedDF
              .Object@mgAnnotatedDF <- new("AnnotatedDataFrame",refDF)

              ## metadata
              .Object@metadata <- metadata

              ## featureData
              .Object@featureData <- new("DNAStringSet")#, feature_data)

              ## for initialization of superclasses
              callNextMethod(.Object,
                             ...)
              .Object
          }
)

## making sure new object conforms to class definition
#setValidity("metagenomeAnnotation")

## TODO - format for user friendly output
setMethod("show", "metagenomeAnnotation",
          function(object){
              metadata <-object@metadata
              print_metadata <- ""
              for(i in names(metadata)){
                  print_metadata <- paste0(print_metadata,
                                           paste0("|", i, ": ",
                                                  metadata[[i]], "\n", sep = ""))
              }
              cat(class(object), "object:\n",
                  "\nMetadata\n",
                  print_metadata,
                  "\nFeature Data:\n",
                  #show(object@featureData), - format output ....
                  "\nAnnotation Data:\n"#,
                  #show(object@mgAnnotatedDF)
              )
              #for use while testing code
              show(object@featureData)
              show(object@mgAnnotatedDF)
          }
)

## Functions
# split_by - splits annotated dataframe by taxonomy
# meTree - generates a tree from annotated dataframe

################################################################################
# ## Old Code
# ## genomicFeatures uses garbage collection to clean up the environment/ memory
# ## prior to setRefClass() would this be appropriate here as well?
# # gc()
#
#
# ## metagenomeAnnotation class definition ----------------------------------------------
# # metagenomeAnnotation <- setRefClass("metagenomeAnnotation", contains="ShortRead",
# #                                      fields=list(taxa="ANY", #use of any as dplyr tbl is not a recognized class
# #                                                  seq="ShortRead",
# #                                                  features="list"
# #                                      ))
# .metagenomeAnnotation <- setRefClass("metagenomeAnnotation",
#                                     #contains=c("ShortRead", "AnnotatedDataFrame"),
#                                     representation( data="environment",
#                                                     annotation="list")
#                                     )
#
# ## MetagenomeAnnotation Validity -----------------------------------------------
# ## Note the object may not necessarily have both ShortRead and taxa data.frame
#
# setValidity("metagenomeAnnotation", function(object) {
#     msg <- NULL
#     if(!(all(sapply(object@data, class) == "DataFrame")))
#         msg <- paste(msg,
#                      "All objects in 'objects@data' has to be of class 'DataFrame'",
#                      sep = "\n")
#
# #     if(!("seq" %in% ls(object)) || !is(object@seq, "ShortRead"))
# #         msg <- paste(msg, "'seq' slot must contain a ShortRead object with sequence data", sep = "\n")
# #     if(!("taxa" %in% ls(object)) || !is(object@taxa, "AnnotatedDataFrame"))
# #         msg <- paste(msg, "'taxa' slot must contain a tbl_sqlite object with taxonomy data", sep = "\n")
# #     if(!("metadata" %in% ls(object)) || !is(object@metadata, "list"))
# #         msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
#     if (is.null(msg)) TRUE else msg
# })
#
# setMethod("show", "metagenomeAnnotation", function(object) {
#     cat("metagenomeAnnotation object\n")
#     .show.annotation(object@annotation)
#     .show.availableAnnotation(object)
# })
#
#
# ## MetagenomeAnnotation Class Constructor --------------------------------------
# metagenomeAnnotation <- function(listOf, annotation = ""){
#     # from minfi IlluminaMethylationAnnotation class - modify for metagenomeAnnotation
# #     stopifnot(annotation != "")
# #     stopifnot(all(c("array", "annotation", "genomeBuild") %in% names(annotation)))
# #     stopifnot(all(c("Manifest", "Locations") %in% names(listOfObjects)))
# #     Manifest <- listOfObjects[["Manifest"]]
# #     stopifnot(setequal(names(Manifest),
# #                        c("Name", "AddressA", "AddressB", "ProbeSeqA",
# #                          "ProbeSeqB", "Type", "NextBase", "Color")))
# #     stopifnot(all(sapply(listOfObjects, class) %in% c("DataFrame", "data.frame")))
# #     stopifnot(all(nrow(Manifest) == sapply(listOfObjects, nrow)))
# #     stopifnot(all(sapply(listOfObjects, function(obj) {
# #         all(rownames(obj) == rownames(Manifest))
# #     })))
# #     stopifnot(all(c("chr", "pos") %in% names(listOfObjects[["Locations"]])))
# #     stopifnot(all(listOfObjects[["Locations"]]$chr %in% .seqnames.order.all))
# #     available <- .availableAnnotation(listOfObjects)
# #     stopifnot(all(defaults %in% names(listOfObjects)))
# #     stopifnot(!anyDuplicated(sub("\\..*", "", defaults)))
#     ## FIXME: Check column names of any Islands object
#     ## Instantiating
#     data <- new.env(parent = emptyenv())
#     for(nam in names(listOfObjects)) {
#         cat(nam, "\n")
#         assign(nam, as(listOfObjects[[nam]], "DataFrame"), envir = data)
#     }
#     ## TODO - look into locking environment
#     lockEnvironment(data, bindings = TRUE)
#     anno <- new("metagenomeAnnotation",
#                 annotation = annotation, data = data)
#     anno
# }
#
# ## metagenomeAnnotation methods -------------------------------------------------------
