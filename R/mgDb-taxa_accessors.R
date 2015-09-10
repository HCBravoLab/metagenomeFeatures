### ============================================================================
##
##                              MgDb Taxa methods
##
### ============================================================================

## See post for functional programming interface for refClass methods http://stackoverflow.com/questions/20649973/functional-interfaces-for-reference-classes
### Not sure how to best document and export MgDb methods, wrote wrappers
### so they can be used using standard function(value) method

### Taxa keys function ---------------------------------------------------------
.taxa_keys <- function(mgdb, keytype){
    nonvalid_keytype <- c()
    for(i in keytype){
        if(!(i %in% taxa_keytypes(mgdb))){
            nonvalid_keytype <- c(nonvalid_keytype, i)
        }
    }
    if(length(nonvalid_keytype) > 0){
        msg <- paste(nonvalid_keytype, "not a valid keytype, use `taxa_keytypes()` for valid keytypes")
        stop(msg)
    }

    mgdb$taxa %>%
        dplyr::select_(keytype) %>%
        dplyr::collect()
}

setGeneric("taxa_keys", signature="mgdb",
           function(mgdb, keytype) standardGeneric("taxa_keys"))

#' Taxonomy values for a given keytype
#'
#' @param mgdb object of MgDb class
#' @param keytype taxonomic classification level
#'
#' @return tbl_df
#' @export
#'
setMethod("taxa_keys", "MgDb",
          function(mgdb, keytype) .taxa_keys(mgdb, keytype))



### Taxa columns function ------------------------------------------------------
.taxa_columns = function(mgdb){
    colnames(mgdb$taxa)
}

setGeneric("taxa_columns", signature="mgdb",
           function(mgdb) standardGeneric("taxa_columns"))

## MgDb taxa_columns method

#' Column names for MgDb taxonomy slot object
#'
#' @param mgdb object of MgDB class
#'
#' @return tbl_df
#' @export
#'
setMethod("taxa_columns", "MgDb",
          function(mgdb) .taxa_columns(mgdb))


### taxa keytypes function -----------------------------------------------------
.taxa_keytypes = function(mgdb){
    colnames(mgdb$taxa)
}

setGeneric("taxa_keytypes", signature="mgdb",
           function(mgdb) standardGeneric("taxa_keytypes"))

## MgDb taxa_keytypes method

#' Column names for MgDb taxonomy slot object
#'
#' @param mgdb object of MgDB class
#'
#' @return tbl_df
#' @export
#'
setMethod("taxa_keytypes", "MgDb",
          function(mgdb) .taxa_keytypes(mgdb))
