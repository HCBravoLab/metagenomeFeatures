################################################################################
##
##                              MgDb_annotateFeatures method
##
################################################################################
## mgDb_annotateFeatures -------------------------------------------------------

## Generates an mgFeatures object when passed a vector of keys or dataframe
## (e.g. query)

.mgDb_annotateFeatures <- function(mgdb, query) {
    ##### SELECT_KEYS
    ## check query type: vector or data.frame
    ## process as vector of database keys or data.frame with column Keys
    if (is.null(query)) {
        stop("query not provided, see documentation for query requirements")
    }

    if (is.data.frame(query)) {
        if (is.element("Keys", colnames(query))) {
            # process Keys column as character
            query$Keys <- as.character(query$Keys)
            select_keys <- query$Keys
        } else {
            stop("Need 'Keys' column in 'query' with database ids")
        }
    } else if (is.vector(query)) { # db_keys used
        select_keys <- as.character(query)
    } else {## query not a data frame or vector
        stop("query must be a vector or data.frame")
    }


    #### FILTERED_DB
    filtered_db <- mgDb_select(mgdb, type = "all",
                               keys = select_keys,
                               keytype = "Keys")

    ##### ANNOTATED_DB
    if (is.data.frame(query)) {
        annotated_db <- dplyr::right_join(query, filtered_db$taxa)
    } else {
        annotated_db <- as.data.frame(filtered_db$taxa)
    }

    #### ANNO_METADATA
    anno_metadata <- mgDb_meta(mgdb)

    # CREATE mgFeatures Object
    new("mgFeatures",
        DataFrame(annotated_db),
        metadata = anno_metadata,
        refDbSeq = filtered_db$seq,
        refDbTree = filtered_db$tree
    )
}




#' Annotating metagenome data with taxonomic information
#'
#' This method is used to create a \linkS4class{mgFeatures} class object
#'
#' @param mgdb MgDb class object
#' @param query A data frame with experimental data to annotate with taxonomic
#'   information, must include column named "Key" with databse ids. Or a vector
#'   of database Keys of entries to include in mgFeatures-class object.
#' @param ... additional arguments passed to select function
#' @return mgFeatures-class object
#' @examples
#' ## MgDb with mock community ids
#' gg85 <- get_gg13.8_85MgDb()
#' ## generating mgFeatures object
#' data(mock_query_df)
#' mock_mgF <- annotateFeatures(gg85, mock_query_df)
#'
#' @rdname annotateFeatures-MgDb-method
setGeneric("annotateFeatures", signature = "mgdb",
           function(mgdb, ...) {
               standardGeneric("annotateFeatures")}
)

#' @export
#' @aliases annotateFeatures,MgDb-method
#' @rdname annotateFeatures-MgDb-method
setMethod("annotateFeatures", "MgDb",
          function(mgdb, query){
              .mgDb_annotateFeatures(mgdb, query)}
)
