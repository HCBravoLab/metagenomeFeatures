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
    if(length(keytype) > 0){
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
            dplyr::collect() %>%
            return()
    }else{
        mgdb$taxa
    }
}

#' Taxonomy values for a given keytype
#'
#' @param mgdb object of MgDb class
#' @param keytype taxonomic classification level
#'
#' @return tbl_df
#' @examples
#' \dontrun{
#' ## need to install greengenes13.5MgDb from github
#' ## https://github.com/HCBravoLab/greengenes13.5MgDb
#' ## untill package is released on bioconductor
#' library(greengenes13.5MgDb)
#' taxa_keys(gg13.5MgDb, keytype = "Phylum")
#' }
#' @family MgDb_accessors
setGeneric("taxa_keys", signature="mgdb",
           function(mgdb, keytype) standardGeneric("taxa_keys"))

#' @describeIn taxa_keys
#' @export
setMethod("taxa_keys", "MgDb",
          function(mgdb, keytype) .taxa_keys(mgdb, keytype))



### Taxa columns function ------------------------------------------------------
.taxa_columns = function(mgdb){
    colnames(mgdb$taxa)
}
#' Column names for MgDb taxonomy slot object
#'
#' @param mgdb object of MgDB class
#'
#' @return tbl_df
#' @note Same function as \code{\link{taxa_keytypes}}.
#' @examples
#' \dontrun{
#' ## need to install greengenes13.5MgDb from github
#' ## https://github.com/HCBravoLab/greengenes13.5MgDb
#' ## untill package is released on bioconductor#' library(greengenes13.5MgDb)
#' taxa_columns(gg13.5MgDb)
#' }
#' @family MgDb_accessors
setGeneric("taxa_columns", signature="mgdb",
           function(mgdb) standardGeneric("taxa_columns"))

## MgDb taxa_columns method


#' @export
#' @describeIn taxa_columns
setMethod("taxa_columns", "MgDb",
          function(mgdb) .taxa_columns(mgdb))


### taxa keytypes function -----------------------------------------------------
.taxa_keytypes = function(mgdb){
    colnames(mgdb$taxa)
}

#' Column names for MgDb taxonomy slot object
#'
#' @param mgdb object of MgDB class
#'
#' @return tbl_df
#' @examples
#' \dontrun{
#' ## need to install greengenes13.5MgDb from github
#' ## https://github.com/HCBravoLab/greengenes13.5MgDb
#' ## untill package is released on bioconductor#' library(greengenes13.5MgDb)
#' taxa_keytypes(gg13.5MgDb)
#' }
#' @family MgDb_accessors
setGeneric("taxa_keytypes", signature="mgdb",
           function(mgdb) standardGeneric("taxa_keytypes"))

## MgDb taxa_keytypes method

#' @export
#' @describeIn taxa_keytypes
setMethod("taxa_keytypes", "MgDb",
          function(mgdb) .taxa_columns(mgdb))
