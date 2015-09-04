## ========================= metagenomeAnnotation Class ========================
## the code below is modeled after the TxDb class in GenomicFeatures package
## this object is generated from mgDb class and contains AnnotatedDataFrame object,
## sequence/ features for the experiment and metadata about the experiment

## metagenomeAnnotation Class

## Annotated Dataframe
##  User provided ids joined with ref db
##  annotation metadata - ref db package, mapping method,
##  user defined features, e.g. annotate function parameters
##  feature data - cluster sequences, cluster IDs

#' annotated marker gene sequence object class
#'
#' @aliases mgAnno
#' @slot mgAnnotatedDF AnnotatedDataFrame
#' @slot metadata list
#' @slot featureData DNAStringSet
#' @return metagenomeAnnotation class object
#' @export
setClass("metagenomeAnnotation",
         representation = list(mgAnnotatedDF = "AnnotatedDataFrame",
                               metadata = "list",
                               featureData = "DNAStringSet"),
         contains = c("AnnotatedDataFrame","DNAStringSet"),
         prototype = prototype(
             mgAnnotatedDF = new("AnnotatedDataFrame"),
             metadata = list(),
             featureData = new("DNAStringSet"))
)

## for use in creating a new object
setMethod("initialize","metagenomeAnnotation",
          function(.Object,refDF,metadata, feature_data, ...){
              ## mgAnnoatedDF
              if(class(refDF) == "AnnotatedDataFrame"){
                  .Object@mgAnnotatedDF
              }else{
                  .Object@mgAnnotatedDF <- new("AnnotatedDataFrame",data = refDF)
              }

              ## metadata
              .Object@metadata <- metadata

              ## featureData
              .Object@featureData <- new("DNAStringSet", feature_data)

              ## for initialization of superclasses
              #callNextMethod(.Object,...)
              .Object
          }
)

## making sure new object conforms to class definition
setValidity("metagenomeAnnotation", function(object) {
    msg <- NULL
    if(!("featureData" %in% ls(object)) || !is(object@featureData, "DNAStringSet"))
        msg <- paste(msg, "'featureData' slot must contain a DNAStringSeq object with sequence data", sep = "\n")
    if(is.null(names(object@featureData)))
        msg <- paste(msg, "'featureData' slot must contain a named DNAStringSet object",sep="\n")
    if(!("mgAnnotatedDF" %in% ls(object)) || !is(object@mgAnnotatedDF, "AnnotatedDataFrame"))
        msg <- paste(msg, "'taxa' slot must contain a tbl_sqlite object with taxonomy data", sep = "\n")
    if(!("metadata" %in% ls(.self)) || !is(.self@metadata, "list"))
        msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
    if (is.null(msg)) TRUE else msg
})


#' @export
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
# split_by - splits metagenomeAnnotation object into a list of metagenomeAnnotation
# objects for each taxa in a specified taxonomic level
## user provides a metagenomeAnnotation object,
## and defines the taxonomy level (e.g. Phylum, Class, Order ect. )to split the object by

.split_by <- function(mgAnno, taxa_level) {
                split_mgAnnoList <- list()
                for( tax in unique(mgAnno@mgAnnotatedDF[[taxa_level]])){
                    annotated_db <- mgAnno@mgAnnotatedDF[mgAnno@mgAnnotatedDF[[taxa_level]] == tax,]

                    query_id <- mgAnno@mgAnnotatedDF$query_id[mgAnno@mgAnnotatedDF[[taxa_level]] == tax]
                    feature_data <- mgAnno@featureData[names(mgAnno@featureData) %in% query_id,]

                    anno_metadata <- mgAnno@metadata
                    anno_metadata$split_by <- c(level = taxa_level, taxa = tax)

                    split_mgAnnoList[[tax]] <- new("metagenomeAnnotation",
                                                   refDF = annotated_db,
                                                   metadata = anno_metadata,
                                                   feature_data = feature_data
                    )
                }
                return(split_mgAnnoList)
}

setGeneric("split_by", function(mgAnno, taxa_level) {
    standardGeneric("split_by")
})

#' Split metagenomeAnnotation Object by Taxa
#'
#' @param mgAnno metagenomeAnnotation class object
#' @param taxa_level taxonomic level used to split the metagenomeAnnotation object at
#'
#' @return list of metagenomeAnnotation objects
#' @export
#'
setMethod("split_by", "metagenomeAnnotation",
          function(mgAnno, taxa_level){
              .split_by(mgAnno, taxa_level)
          }
)
# mgTree - generates a tree from annotated dataframe
