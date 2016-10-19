################################################################################
##
##                              MgDb_annotateFeatures method
##
################################################################################
## mgDb_annotateFeatures --------------------------------------------------------------------

## Generates an mgFeatures object when passed a vector of keys or dataframe
## (e.g. query)

.mgDb_annotateFeatures <- function(mgdb, query) {

	#SELECT_KEYS

	# check query type: vector or data.frame
	# process as vector of database keys or data.frame with column Keys

	if (is.data.frame(query)) {

		if (is.element("Keys", colnames(query))) {

			query$Keys <- as.character(query$Keys) # process Keys column as character
			select_keys <- query$Keys

		} else {

			stop("Need 'Keys' column in 'query' with database ids")

		}

	} else if (is.vector(query)) { # db_keys used

		select_keys <- as.character(query)

	} else { # query_df and db_keys both null

		stop("query not a vector or data.frame, see documentation for query requirements")

	}


	# FILTERED_DB

	filtered_db <- mgDb_select(mgdb, type = "all",
								keys = select_keys,
								keytype = "Keys")

	# ANNOTATED_DB

	if (!is.null(query)) { # using query_df

		annotated_db <- dplyr::right_join(query, filtered_db$taxa)

	} else if (!is.null(db_keys)) { # using db_keys

		annotated_db <- as.data.frame(filtered_db$taxa)

	} else { # sanity check -- should not enter here

		stop("issue: no query")
	}

	# ANNO_METADATA

	anno_metadata <- mgdb$metadata

    # CREATE mgFeatures Object
	new("mgFeatures",
		data = annotated_db,
		metadata = anno_metadata,
		refDbSeq=filtered_db$seq,
		refDbTree=filtered_db$tree
		)
}



 #' Annotating metagenome data with taxonomic information
 #'
 #' This method is used to create a \linkS4class{mgFeatures} class
 #' object
 #'
 #' @param mgdb MgDb class object
 #' @param query A data frame with experimental data to annotate with
 #'   taxonomic information, must include column named "Key" with databse ids.
 #'   Or a vector of database Keys of entries to include in mgFeatures-class object.
 #' @param ... additional arguments passed to select function
 #' @return mgFeatures-class object
 #' @examples
 #' ## MgDb with mock community ids
 #' mockMgDb <- get_mockMgDb()
 #' ## generating mgFeatures object
 #' data(mock_query_df)
 #' mock_mgF <- annotateFeatures(mockMgDb, mock_query_df)
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
