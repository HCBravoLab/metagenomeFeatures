### ============================================================================
##
##                              MgDb Taxa methods
##
### ============================================================================

### Taxa keys function ---------------------------------------------------------
.taxa_keys <- function(mgdb, keytype){
    if (length(keytype) > 0) {
        nonvalid_keytype <- c()
        for (i in keytype) {
            if (!(i %in% taxa_keytypes(mgdb))) {
                nonvalid_keytype <- c(nonvalid_keytype, i)
            }
        }
        if (length(nonvalid_keytype) > 0) {
            msg <- paste(nonvalid_keytype,
                         "not a valid keytype,", "
                         use `taxa_keytypes()` for valid keytypes")
            stop(msg)
        }
        mgdb_taxa(mgdb) %>%
            dplyr::select_(keytype) %>%
            dplyr::collect() %>%
            return()
    }else{
        mgdb_taxa(mgdb)
    }
}

#' Taxonomy values for a given keytype
#'
#' @name taxa_keys
#' @param mgdb object of MgDB class
#' @param keytype taxonomic classification level
#' @return tbl_df
#'
#' @examples
#' demoMgDb <- get_demoMgDb()
#' taxa_keys(demoMgDb, keytype = "Phylum")
#' @exportMethod taxa_keys
#' @rdname taxa_keys
setGeneric("taxa_keys", signature = "mgdb",
           function(mgdb, keytype) standardGeneric("taxa_keys"))

## MgDb taxa_keys method
#' @rdname taxa_keys
#' @aliases taxa_keys,MgDb-method
setMethod("taxa_keys", "MgDb",
          function(mgdb, keytype) .taxa_keys(mgdb, keytype))



### Taxa columns function ------------------------------------------------------
.taxa_columns = function(mgdb){
    taxa_cols <- colnames(mgdb_taxa(mgdb))
    ## Excluding decipher column names
    decipher_cols <- c("row_names", "identifier", "description")
    taxa_cols[!(taxa_cols %in% decipher_cols)]
}

#' Column names for MgDb taxonomy slot object
#'
#' @name taxa_columns
#' @param mgdb object of MgDB class
#' @return character vector
#' @note Same function as \code{\link{taxa_keytypes}}.
#' @examples
#' demoMgDb <- get_demoMgDb()
#' taxa_columns(demoMgDb)
#' @exportMethod taxa_columns
#' @rdname taxa_columns
setGeneric("taxa_columns", signature = "mgdb",
           function(mgdb) standardGeneric("taxa_columns"))

## MgDb taxa_columns method
#' @aliases taxa_columns,MgDb-method
#' @rdname taxa_columns
setMethod("taxa_columns", "MgDb",
          function(mgdb) .taxa_columns(mgdb))


### taxa keytypes function -----------------------------------------------------
.taxa_keytypes = function(mgdb){
    colnames(mgdb_taxa(mgdb))
}

#' Column names for MgDb taxonomy slot object
#'
#' @name taxa_keytypes
#' @param mgdb object of MgDB class
#' @return tbl_df
#' @examples
#' demoMgDb <- get_demoMgDb()
#' taxa_keytypes(demoMgDb)
#' @exportMethod taxa_keytypes
#' @rdname taxa_keytypes
setGeneric("taxa_keytypes", signature = "mgdb",
           function(mgdb) standardGeneric("taxa_keytypes"))

## MgDb taxa_keytypes method

## MgDb taxa_keytypes method
#' @aliases taxa_keytypes,MgDb-method
#' @rdname taxa_keytypes
setMethod("taxa_keytypes", "MgDb",
          function(mgdb) .taxa_keytypes(mgdb))
