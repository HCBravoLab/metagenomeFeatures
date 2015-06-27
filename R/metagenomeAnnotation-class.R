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
              .Object@mgAnnotatedDF <- new("AnnotatedDataFrame",
                                           data = refDF)

              ## metadata
              .Object@metadata <- metadata

              ## featureData
              .Object@featureData <- new("DNAStringSet")#, feature_data)

              ## for initialization of superclasses
              callNextMethod(.Object,...)
              .Object
          }
)

## making sure new object conforms to class definition
setValidity("metagenomeAnnotation", function(object) {
    msg <- NULL
    if(!("featureData" %in% ls(object)) || !is(object@featureData, "DNAStringSet"))
        msg <- paste(msg, "'featureData' slot must contain a DNAStringSeq object with sequence data", sep = "\n")
    if(!("mgAnnotatedDF" %in% ls(object)) || !is(object@mgAnnotatedDF, "AnnotatedDataFrame"))
        msg <- paste(msg, "'taxa' slot must contain a tbl_sqlite object with taxonomy data", sep = "\n")
    if(!("metadata" %in% ls(.self)) || !is(.self@metadata, "list"))
        msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
    if (is.null(msg)) TRUE else msg
})


## %%TODO%% - format for user friendly output
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

################################# Methods ######################################

# split_by - splits annotated dataframe by taxonomy
# meTree - generates a tree from annotated dataframe
