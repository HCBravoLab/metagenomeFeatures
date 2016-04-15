## ========================= metagenomeAnnotation Class ========================
## the code below is modeled after the TxDb class in GenomicFeatures package
## this object is generated from mgDb class and contains AnnotatedDataFrame
## object, sequence/ features for the experiment and metadata about the
## experiment

## metagenomeAnnotation Class

## Annotated Dataframe
##  User provided ids joined with ref db
##  annotation metadata - ref db package, mapping method,
##  user defined features, e.g. annotate function parameters
##  feature data - cluster sequences, cluster IDs
# setOldClass("phylo")
# #' metagenomeAnnotation-class object
# #'
# #' Object contains taxonomic annotation and sequence data for an experiment .
# #' The class extends the \link[Biobase]{AnnotatedDataFrame} class with a slot
# #' for metadata including information on the database source and methods used to
# #' perform the taxonomic assignment. Additionally, the user can include
# #' experiment sequence data in the object for use in downstream analysis.
# #'
# #' @slot metadata list
# #' @slot referenceDbSeqData DNAStringSet
# #' @slot referenceDbTreeData phylo
# #' @return metagenomeAnnotation class object
# #' @export
# #' @examples
# #' data(msd16s_metagenomeAnnotation)
# #' @rdname metagenomeAnnotation-class
# setClass("metagenomeAnnotation",
#         slots = list(metadata = "list",
#                      referenceDbSeqData="DNAStringSet",
#                      referenceDbTreeData="phylo"),
#         contains = c("AnnotatedDataFrame"),
#         prototype = new("VersionedBiobase",
#                         versions = c(classVersion("AnnotatedDataFrame"),
#                                      metagenomeAnnotation = "1.0.1"))
# )

## making sure new object conforms to class definition
# setValidity("metagenomeAnnotation", function(object) {
#      msg <- NULL
#      if(!("experimentSeqData" %in% slotNames(object)) ||
#         !is(object@experimentSeqData, "DNAStringSet"))
#          msg <- paste(msg,
#                       "'experimentSeqData' slot must be a DNAStringSeq object",
#                       sep = "\n")
#      if(!("referenceDbSeqData" %in% slotNames(object)) ||
#         !is(object@experimentSeqData, "DNAStringSet"))
#          msg <- paste(msg,
#                       "'referenceDbSeqData' slot must be a DNAStringSeq object",
#                       sep = "\n")
#      # if(!("experimentTreeData" %in% slotNames(object)) ||
#      #    !is(object@experimentSeqData, "phylo"))
#      #     msg <- paste(msg,
#      #                  "'experimentTreeData' slot must be a phylo object",
#      #                  sep = "\n")
#      if(!("metadata" %in% slotNames(object)) || !is(object@metadata, "list"))
#          msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
#      if (is.null(msg)) TRUE else msg
# })

################################# Methods ######################################
#split_by - splits metagenomeAnnotation object into a list of
#metagenomeAnnotation objects for each taxa in a specified taxonomic level #
#user provides a metagenomeAnnotation object, # and defines the taxonomy level
#(e.g. Phylum, Class, Order ect. )to split the object by

# .split_by <- function(mgAnno, taxa_level) {
#     split_mgAnnoList <- list()
#     for( tax in unique(mgAnno@data[[taxa_level]])){
#         annotated_db <- mgAnno@data[mgAnno@data[[taxa_level]] == tax,]
#
#         query_id <- mgAnno@data$query_id[mgAnno@data[[taxa_level]] == tax]
#         experimentSeqData <- mgAnno@experimentSeqData[names(mgAnno@experimentSeqData) %in% query_id,]
#
#         anno_metadata <- mgAnno@metadata
#         anno_metadata$split_by <- list(level = taxa_level, taxa = tax)
#
#         split_mgAnnoList[[tax]] <- new("metagenomeAnnotation",
#                                        annotated_db,
#                                        metadata = anno_metadata,
#                                        experimentSeqData = experimentSeqData
#         )
#     }
#     return(split_mgAnnoList)
# }

# #' Split metagenomeAnnotation Object by Taxa
# #'
# #' Function takes a \link{metagenomeAnnotation-class}
# #' object and splits it based on a user provided taxonomic level, returning a
# #' list of \link{metagenomeAnnotation-class} objects for
# #' each unique taxa at that level.
# #'
# #' @param mgAnno metagenomeAnnotation class object
# #' @param taxa_level taxonomic level used to split the metagenomeAnnotation
# #'   object at
# #' @examples
# #' data(msd16s_metagenomeAnnotation)
# #' split_by(msd16s_metagenomeAnnotation, "Phylum")
# #' @return list of metagenomeAnnotation objects

# setGeneric("split_by", function(mgAnno, taxa_level) {
#     standardGeneric("split_by")
# })

# #' @exportMethod split_by
# #' @describeIn split_by
# setMethod("split_by", "metagenomeAnnotation",
#           function(mgAnno, taxa_level){
#               .split_by(mgAnno, taxa_level)
#           }
# )
