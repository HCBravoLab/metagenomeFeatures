### ============================================================================
##
##                              MgDb Taxa methods
##
### ============================================================================

## See post for functional programming interface for refClass methods http://stackoverflow.com/questions/20649973/functional-interfaces-for-reference-classes
### Not sure how to best document and export MgDb methods, wrote wrappers
### so they can be used using standard function(value) method

### Taxa keys function ---------------------------------------------------------
.taxa_keys <- function(x, keytype){
    x$taxa %>%
        dplyr::select_(keytype) %>%
        dplyr::collect()
}

setGeneric("taxa_keys", signature="x",
           function(x, ...) standardGeneric("taxa_keys"))

#' Taxonomy values for a given keytype
#'
#' @param mgdb object of MgDB class
#' @param keytype taxonomic classification level
#'
#' @return tbl_df
#' @exportMethod
#'
#' @examples taxa_keys(mgdb, keytype = "Class")
setMethod("taxa_keys", "MgDb",
          function(x, ...) .taxa_keys(x, ...))



### Taxa columns function ------------------------------------------------------
.taxa_columns = function(x){
    colnames(x$taxa)
}

setGeneric("taxa_columns", signature="x",
           function(x) standardGeneric("taxa_columns"))

## MgDb taxa_columns method

#' Column names for MgDb taxonomy slot object
#'
#' @param mgdb object of MgDB class
#' @param keytype taxonomic classification level
#'
#' @return tbl_df
#' @exportMethod
#'
#' @examples taxa_column(mgdb)
setMethod("taxa_columns", "MgDb",
          function(x) .taxa_columns(x))


### taxa keytypes function -----------------------------------------------------
.taxa_keytypes = function(x){
    colnames(x$taxa)
}

setGeneric("taxa_keytypes", signature="x",
           function(x) standardGeneric("taxa_keytypes"))

## MgDb taxa_keytypes method

#' Column names for MgDb taxonomy slot object
#'
#' @param mgdb object of MgDB class
#' @param keytype taxonomic classification level
#'
#' @return tbl_df
#' @exportMethod
#'
#' @examples taxa_keytypes(mgdb)
setMethod("taxa_keytypes", "MgDb",
          function(x) .taxa_keytypes(x))
