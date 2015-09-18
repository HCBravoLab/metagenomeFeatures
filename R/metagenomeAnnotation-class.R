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

#' metagenomeAnnotation-class object
#' Object contains marker gene sequence object with taxonomic annotation data
#' for set of sequences.  The class extends the AnnotatedDataFrame class with
#' a slot for metadata including information on the database source and methods
#' used to perform the taxonomic assignment. Additionally, the user can include
#' cluster sequence experiment sequence data in the object for use in downstream
#' analysis.
#'
#' @slot metadata list
#' @slot experimentSeqData DNAStringSet
#' @return metagenomeAnnotation class object
#' @export
#' @importFrom Biobase classVersion
#' @importClassesFrom Biostrings DNAStringSet
#' @rdname metagenomeAnnotation-class
setClass("metagenomeAnnotation",
        slots = list(metadata = "list", experimentSeqData = "DNAStringSet"),
        contains = c("AnnotatedDataFrame"),
        prototype = new("VersionedBiobase",
                        versions = c(Biobase::classVersion("AnnotatedDataFrame"),
                                     metageenomeAnnotation = "1.0.0"))
)

## making sure new object conforms to class definition
# setValidity("metagenomeAnnotation", function(mgAnno) {
#     msg <- NULL
#     if(!("experimentSeqData" %in% ls(mgAnno)) || !is(mgAnno@experimentSeqData, "DNAStringSet"))
#         msg <- paste(msg,
#                      "'experimentSeqData' slot must contain a DNAStringSeq object with sequence data",
#                      sep = "\n")
#     if(!("metadata" %in% ls(mgAnno)) || !is(mgAnno@metadata, "list"))
#         msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
#     if (is.null(msg)) TRUE else msg
# })


## No longer needed???
# # Display summary of metagenomeAnnotaiton-class object
# # param object metagenomeAnnotation-class object
# #
# # export
# # rdname metagenomeAnnotation-class
# setMethod("show", "metagenomeAnnotation",
#           function(object){
#               metadata <-object@metadata
#               print_metadata <- ""
#               for(i in names(metadata)){
#                   print_metadata <-
#                       paste0(print_metadata,
#                              paste0("|", i, ": ",
#                                     metadata[[i]], "\n", sep = ""))
#               }
#               print("Metadata:")
#               print(print_metadata)
#               print("Feature Data:")
#               show(object@experimentSeqData)
#               print("Annotation Data:")
#               show(object@annotationData)
#           }
# )

################################# Methods ######################################
# split_by - splits metagenomeAnnotation object into a list of metagenomeAnnotation
# objects for each taxa in a specified taxonomic level
## user provides a metagenomeAnnotation object,
## and defines the taxonomy level (e.g. Phylum, Class, Order ect. )to split the object by

.split_by <- function(mgAnno, taxa_level) {
                split_mgAnnoList <- list()
                for( tax in unique(mgAnno@data[[taxa_level]])){
                    annotated_db <- mgAnno@data[mgAnno@data[[taxa_level]] == tax,]

                    query_id <- mgAnno@data$query_id[mgAnno@data[[taxa_level]] == tax]
                    experimentSeqData <- mgAnno@experimentSeqData[names(mgAnno@experimentSeqData) %in% query_id,]

                    anno_metadata <- mgAnno@metadata
                    anno_metadata$split_by <- list(level = taxa_level, taxa = tax)

                    split_mgAnnoList[[tax]] <- new("metagenomeAnnotation",
                                                   annotated_db,
                                                   metadata = anno_metadata,
                                                   experimentSeqData = experimentSeqData
                    )
                }
                return(split_mgAnnoList)
}

#' Split metagenomeAnnotation Object by Taxa
#'
#' @param mgAnno metagenomeAnnotation class object
#' @param taxa_level taxonomic level used to split the metagenomeAnnotation object at
#'
#' @return list of metagenomeAnnotation objects
#' @export
#' @rdname split_by-metagenomeAnnotation-method
setGeneric("split_by", function(mgAnno, taxa_level) {
    standardGeneric("split_by")
})

#' @rdname split_by-metagenomeAnnotation-method
setMethod("split_by", "metagenomeAnnotation",
          function(mgAnno, taxa_level){
              .split_by(mgAnno, taxa_level)
          }
)
# mgTree - generates a tree from annotated dataframe
