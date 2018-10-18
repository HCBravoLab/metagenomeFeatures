## ========================= mgFeature Class ========================
#' Class mgFeature
#'
#' Class defines taxonomic annotation and reference sequence data for
#' classified OTUs. The class extends the \link[S4Vectors]{DataFrame}
#' class with a slot with a subset of the reference database sequences and
#' phylogenetic tree for taxonomically classified OTUs, along with an additional
#' slot for metadata including information on the database source.
#'
#' @return mgFeature class object
#' @name mgFeatures-class
#' @rdname mgFeatures-class
#' @importFrom S4Vectors DataFrame
#' @exportClass mgFeatures
#' @examples
#' data(mock_mgF)
setClass("mgFeatures",
         slots = list(metadata = "list",
                      refDbSeq = "DNAStringSetOrNull",
                      refDbTree = "phyloOrNULL"),
         contains = c("DataFrame")
)

## making sure new object conforms to class definition
setValidity("mgFeatures", function(object) {
    msg <- NULL
    if(!("refDbSeq" %in% slotNames(object)) ||
       (is(object@refDbSeq, "DNAStringSet") && is(object@refDbSeq, "NULL")))
        msg <- paste(msg,
                     "'refDbSeq' slot must be a DNAStringSeq or NULL object",
                     sep = "\n")
    if(!("refDbTree" %in% slotNames(object)) ||
       (is(object@refDbTree, "phylo") && is(object@refDbTree, "NULL")))
        msg <- paste(msg,
                     "'refDbTree' slot must be a phylo or NULL object",
                     sep = "\n")
    if(!("metadata" %in% slotNames(object)) || !is(object@metadata, "list"))
        msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
    if (is.null(msg)) TRUE else msg
})



#' mgFeatures-class constructor
#' @param taxa a DataFrame-class or object that can be coerced into a DataFrame
#' @param tree a phylo-class object with phylogenetic tree
#' @param metadata a list
#' @param seq DNAStringSet-object with feature sequences
#' @return mgFeatures-class object
#' @name mgFeatures-class
#' @rdname mgFeatures-class
#' @export
#'
#' @examples
#' mgFeatures(taxa = data.frame(), metadata = list())
mgFeatures <- function(taxa = data.frame(), tree = NULL, seq = NULL, metadata){
    new("mgFeatures",
        DataFrame(taxa),
        metadata = metadata,
        refDbSeq = seq,
        refDbTree = tree
    )
}


################################################################################
################################################################################
##
##                              mgFeatures Methods
##
################################################################################
################################################################################

## subset ----------------------------------------------------------------------
#' @importFrom ape drop.tip
.subset_tree <- function(tree, ids){
    drop_tips <- tree$tip.label[!(tree$tip.label %in% ids)]
    # drop.tip return class phy defining class to match mgFeature class
    # description
    drop.tip(tree,drop_tips) # %>% ape::as.phylo()
}

#' mgFeatures-class subset method
#' @param x Object to extract elements
#' @param i,j element indices to extract or replace
#' @param ... other parameters to subset function
#' @param drop default to FALSE
#' @name mgFeatures-methods
#' @rdname mgFeatures-methods
#' @aliases [,mgFeatures-method
setMethod("[", "mgFeatures",
          function(x, i, j, ..., drop = FALSE) {
              obj = callNextMethod()

              ## Letting subset call to DataFrame define subset rows
              ids <- rownames(obj)

              ## Subsetting tree
              obj@refDbTree <- .subset_tree(obj@refDbTree, ids)

              ## Subsetting seq
              obj@refDbSeq <- obj@refDbSeq[names(obj@refDbSeq) %in% ids]

              ## Return updated object
              obj
          }
)

## accessors -------------------------------------------------------------------

#' mgFeatures accessors
#'
#' Accessors for \linkS4class{mgFeatures}-class object slots.\cr
#' \code{mgF_seq} - refDbSeq slot,\cr
#' \code{mgF_taxa} - taxa slot,\cr
#' \code{mgF_tree} - phylogenetic tree slot, and\cr
#' \code{mgF_meta} - metadata slot.\cr
#'
#' @param mgF mgFeatures-class object.
#'
#' @return appropriate class object for the slot accessed
#' @name mgFeatures-accessors
#' @rdname mgFeatures-accessors
#' @examples
#' data(mock_mgF)
#' mgF_seq(mock_mgF)
#' mgF_taxa(mock_mgF)
#' mgF_tree(mock_mgF)
#' mgF_meta(mock_mgF)
NULL

#' Extract \code{mgF_tree} - phylogenetic tree slot
#' @name mgFeatures-accessors
#' @rdname mgFeatures-accessors
#' @export
mgF_tree <- function(mgF){
    mgF@refDbTree
}

#' Extract  \code{mgF_seq} - - refDbSeq slot
#' @name mgFeatures-accessors
#' @rdname mgFeatures-accessors
#' @export
mgF_seq <- function(mgF){
    mgF@refDbSeq
}

## Note using DataFrame the taxa information is stored as lists and not a data
# frame. Function returns a DataFrame but no assignment function is defined.

#' Extract \code{mgF_taxa} - taxa slot
#' @name mgFeatures-accessors
#' @rdname mgFeatures-accessors
#' @export
mgF_taxa <- function(mgF){
    DataFrame(mgF)
}

#' Extract \code{mgF_meta} - metadata slot
#' @name mgFeatures-accessors
#' @rdname mgFeatures-accessors
#' @export
mgF_meta <- function(mgF){
    mgF@metadata
}


