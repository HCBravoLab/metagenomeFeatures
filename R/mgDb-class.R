################################################################################
##
## Definition and methods for MgDb class
## using RefClass as db changes state with query string
##
################################################################################

## MgDb Class -----------------------------------------------------------------------------

## loading the sqlite database from file
.load_taxa_db <- function(taxdb){
    db_con <- dplyr::src_sqlite(taxdb, create = FALSE)
    dplyr::tbl(src = db_con, from = "taxa")
}

## Tree can be either rds with phylo class object or tree file
.load_tree <- function(tree_file){
    if(grepl("rds",tree_file)){
        tree <- readRDS(tree_file)
    }else{
        tree <- ape::read.tree(tree_file)
    }
    tree
}

setOldClass(c("tbl_sqlite"))
#' Metagenome Database class
#'
#' The MgDb-class object contains sequence and taxonomic data for a 16S rRNA
#' taxonomic database, see the \pkg{greengenes13.5MgDb} package as an example
#' database. The \code{get_demoMgDb} function exports a small subset of the
#' database in \pkg{greengenes13.5MgDb}\pkg{metagenomeFeatures} package as an
#' example of a MgDb-class object.
#' @aliases mgdb
#' @field taxa taxonomic information for database sequences
#' @field seq database reference sequences
#' @field tree reference phylogenetic tree
#' @field taxa_file name of sqlite db
#' @field tree_file name of phylogenetic tree file
#' @field metadata associated metadata for the database
#' @export
#' @usage # library(greengenes13.5MgDb)
#' @examples
#' # example MgDb-class object, a small subset of the Greengenes 13.5 database.
#' get_demoMgDb()
#' @note Currently the only database with a MgDb package is the
#'   \href{http://greengenes.secondgenome.com/}{Greengenes database} (version
#'   13.5), additional packages are planned.
#' @rdname MgDb-class
#' @importFrom dplyr tbl_sql
MgDb <- setRefClass("MgDb",
                    #contains="DNAStringSet"
                    fields=list(seq="DNAStringSet",
                                # add seq file inplace of reading DNAStringSet
                                taxa = "tbl_sqlite",
                                taxa_file = "character",
                                tree_file = "character",
                                tree = "phyloOrNULL",
                                metadata= "list"),
                    methods=list(
                        initialize=function(...){
                            callSuper(...)
                            #if(!exists("taxa")){
                            taxa <<- .load_taxa_db(taxa_file)
                            #} else {
                            #    taxa <<- taxa
                            #}

                            seq <<- seq
                            if(tree_file != "not available"){
                                tree <<- .load_tree(tree_file)
                            }
                            metadata <<- metadata
                        }))

## Validity ---------------------------------------------------------------

setValidity("MgDb", function(object) {
    msg <- NULL
    if(!("seq" %in% ls(object)) || !is(object$seq, "DNAStringSet"))
        msg <- paste(msg,
                     "'seq' slot must contain DNAStringSeq object",
                     sep = "\n")
    if(!("taxa" %in% ls(object)) || !is(object$taxa, "tbl_sqlite"))
        msg <- paste(msg,
                     "'taxa' slot must contain a tbl object",
                     sep = "\n")
    if(!("tree" %in% ls(object)) ||
       (is(object@tree, "phylo") && is(object@tree, "NULL")))
        msg <- paste(msg,
                     "'tree' slot must contain a phyloOrNULL object",
                     sep = "\n")
    if(!("metadata" %in% ls(object)) || !is(object$metadata, "list"))
        msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
    if (is.null(msg)) TRUE else msg
})

################################################################################
################################################################################
##
##                              MgDb Methods
##
################################################################################
################################################################################

## Show ------------------------------------------------------------------------

#' Display summary of MgDb-class object
#' @param object MgDb-class object
#' @return MgDb-class summary

#' @export
setMethod("show", "MgDb",
          function(object){
              cat(class(object), "object:")
              print("Metadata")
              metadata <- object $metadata
              for(i in names(metadata)){
                  cat("|", i, ": ", metadata[[i]], "\n", sep = "")
              }
              print("Sequence Data:")
              print(object$seq)
              print("Taxonomy Data:")
              print(object$taxa)
              print("Tree Data:")
              print(object$tree)
          }
)

## Accessors -------------------------------------------------------------------

#' MgDb tree slot accessor
#'
#' @param mgdb  MgDb class object
#'
#' @return phylo class object
#' @export
#'
#' @examples
#' demoMgDb <- get_demoMgDb()
#' mgdb_tree(demoMgDb)
mgdb_tree <- function(mgdb){
    mgdb$tree
}

#' MgDb seq slot accessor
#'
#' @param mgdb  MgDb class object
#'
#' @return DNAStringSet class object
#' @export
#'
#' @examples
#' demoMgDb <- get_demoMgDb()
#' mgdb_seq(demoMgDb)
mgdb_seq <- function(mgdb){
    mgdb$seq
}

#' MgDb taxa slot accessor
#'
#' @param mgdb  MgDb class object
#'
#' @return tbl_sql connection to sqlite table
#' @export
#'
#' @examples
#' demoMgDb <- get_demoMgDb()
#' mgdb_taxa(demoMgDb)
mgdb_taxa <- function(mgdb){
    mgdb$taxa
}

#' MgDb metadata slot accessor
#'
#' @param mgdb  MgDb class object
#'
#' @return list
#' @export
#'
#' @examples
#' demoMgDb <- get_demoMgDb()
#' mgdb_meta(demoMgDb)
mgdb_meta <- function(mgdb){
    mgdb$metadata
}
