### ============================================================================
##
##                              MgDb Taxa methods
##
### ============================================================================

#' MgDb-class Taxa slot helper functions
#'
#' Helper functions for for \linkS4class{MgDb}-class taxa slot.
#' \code{taxa_columns} - taxa slot column names, \code{taxa_keytypes} - taxa
#' slot keytypes (values used with \code{taxa_columns} and \code{mgDb_select}
#' functions), and \code{taxa_keys} - database values for a specific keytype.
#'
#' @name taxa_
#' @param mgdb MgDb-class object.
#' @param keytype character string specifying keys to return
#'
#' @return appropriate class object for the slot accessed
#' @examples
#' gg85 <- get_gg13.8_85MgDb()
#' taxa_columns(gg85)
#' taxa_keytypes(gg85)
#' taxa_keys(gg85, keytype = "Phylum")
NULL


### Taxa keys function ---------------------------------------------------------
# #' Taxa keys functions
#' @importFrom dplyr select_
#' @importFrom dplyr collect
# #' @param mgdb mgDb class
# #' @param keytype taxa keytype
# #' @keywords internals
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
        mgDb_taxa(mgdb) %>%
            select_(keytype) %>%
            collect() %>%
            return()
    }else{
        mgDb_taxa(mgdb)
    }
}

# Taxonomy values for a given keytype
#' @export
#' @rdname taxa_
setGeneric("taxa_keys", signature = "mgdb",
           function(mgdb, keytype) standardGeneric("taxa_keys"))

## MgDb taxa_keys method
#' @rdname taxa_
#' @aliases taxa_keys,MgDb-method
setMethod("taxa_keys", "MgDb",
          function(mgdb, keytype) .taxa_keys(mgdb, keytype))



### Taxa columns function ------------------------------------------------------
# #' taxa columns
# #' @param mgdb mdgdb object
# #' @keywords internals
.taxa_columns = function(mgdb){
    taxa_cols <- colnames(mgDb_taxa(mgdb))
    ## Excluding decipher column names
    decipher_cols <- c("row_names", "identifier", "description")
    taxa_cols[!(taxa_cols %in% decipher_cols)]
}

#' Column names for MgDb taxonomy slot object
#' @export
#' @rdname taxa_
setGeneric("taxa_columns", signature = "mgdb",
           function(mgdb) standardGeneric("taxa_columns"))

## MgDb taxa_columns method
#' @rdname taxa_
#' @aliases taxa_columns,MgDb-method
setMethod("taxa_columns", "MgDb",
          function(mgdb) .taxa_columns(mgdb))


### taxa keytypes function -----------------------------------------------------
# #' @keywords internals
.taxa_keytypes = function(mgdb){
    colnames(mgDb_taxa(mgdb))
}

# Column names for MgDb taxonomy slot object
#' @exportMethod taxa_keytypes
#' @rdname taxa_
setGeneric("taxa_keytypes", signature = "mgdb",
           function(mgdb) standardGeneric("taxa_keytypes"))

## MgDb taxa_keytypes method

## MgDb taxa_keytypes method
#' @aliases taxa_keytypes,MgDb-method
#' @rdname taxa_
setMethod("taxa_keytypes", "MgDb",
          function(mgdb) .taxa_keytypes(mgdb))
