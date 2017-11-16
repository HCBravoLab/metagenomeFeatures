## ========================= mgFeature Class ========================
#' mgFeature-class object
#'
#' Object contains taxonomic annotation and reference sequence data for
#' classified OTUs. The class extends the \link[S4Vectors]{DataFrame}
#' class with a slot with a subset of the reference database sequences and
#' phylogenetic tree for taxonomically classified OTUs, along with an additional
#' slot for metadata including information on the database source.
#'
#' @slot metadata list
#' @slot refDbSeq DNAStringSet
#' @slot refDbTree phyloOrNULL
#' @return mgFeature class object
#' @export
#' @examples
#' data(mock_mgF)
#' @rdname mgFeatures-class
#' @importFrom S4Vectors DataFrame
setClass("mgFeatures",
         slots = list(metadata = "list",
                      refDbSeq="DNAStringSet",
                      refDbTree = "phyloOrNULL"),
         contains = c("DataFrame")
)

## making sure new object conforms to class definition
setValidity("mgFeatures", function(object) {
    msg <- NULL
    if(!("refDbSeq" %in% slotNames(object)) ||
       !is(object@refDbSeq, "DNAStringSet"))
        msg <- paste(msg,
                     "'refDbSeq' slot must be a DNAStringSeq object",
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

################################################################################
################################################################################
##
##                              mgFeatures Methods
##
################################################################################
################################################################################

## subset ----------------------------------------------------------------------
.subset_tree <- function(tree, ids){
    drop_tips <- tree$tip.label[!(tree$tip.label %in% ids)]
    # drop.tip return class phy defining class to match mgFeature class description
    ape::drop.tip(tree,drop_tips) %>% ape::as.phylo()
}

setMethod("[", "mgFeatures",
          function (x, i, j, ..., drop = FALSE) {
              obj = callNextMethod()

              ## Letting subset call to AnnotatedDataFrame define subset rows
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
#' Accessors for \linkS4class{mgFeatures}-class object slots. \code{mgF_seq} - refDbSeq slot, \code{mgF_taxa} - taxa slot, \code{mgF_tree} - phylogenetic tree slot, and \code{mgF_meta} - metadata slot.
#'
#' @name mgF_
#' @param mgF mgFeatures-class object.
#'
#' @return appropriate class object for the slot accessed
#' @examples
#' data(mock_mgF)
#' mgF_seq(mock_mgF)
#' mgF_taxa(mock_mgF)
#' mgF_tree(mock_mgF)
#' mgF_meta(mock_mgF)
NULL

#' @rdname mgF_
#' @export
mgF_tree <- function(mgF){
    mgF@refDbTree
}

#' @rdname mgF_
#' @export
mgF_seq <- function(mgF){
    mgF@refDbSeq
}

## Note using DataFrame the taxa information is stored as lists and not a data
# frame. Function returns a DataFrame but no assignment function is defined.

#' @rdname mgF_
#' @export
mgF_taxa <- function(mgF){
    DataFrame(mgF)
}

#' @rdname mgF_
#' @export
mgF_meta <- function(mgF){
    mgF@metadata
}


