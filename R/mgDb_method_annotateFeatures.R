################################################################################
##
##                              MgDb_annotateFeatures method
##
################################################################################
## mgDb_annotateFeatures --------------------------------------------------------------------

## Generates an mgFeatures object when passed a vector of keys or datafram 
## (e.g. query_df)



.mgDb_annotateFeatures <- function(mgdb, db_keys = NULL, query_df = NULL) {

	#SELECT_KEYS

	# db_keys VS query_df use
	# process db_keys OR query_df 
	# assign SELECT_KEYS

	if (!is.null(query_df)) { # query_df takes precedence
		
		message("Using query_df") # alert usage of query_df over db_keys
		
		# alt if: "Keys" %in% colnames(query_df)
		if (is.element("Keys", colnames(query_df))) { 

			query_df$Keys <- as.character(query_df$Keys) # process Keys column as character
			select_keys <- query_df$Keys

		} else {
			
			stop("Need 'Keys' column in 'query_df' with database ids")

		}

	} else if (!is.null(db_keys)) { # db_keys used

		message("Using db_keys") # alert usage of db_keys over query_df
		select_keys <- as.character(db_keys)

	} else { # query_df and db_keys both null
		
		stop("Need either 'db_keys' or 'query_df'")

	}


	# FILTERED_DB

	filtered_db <- mgDb_select(mgdb, type = "all",
								keys = select_keys,
								keytype = "Keys")
	

	# ANNOTATED_DB

	if (!is.null(query_df)) { # using query_df

		annotated_db <- dplyr::right_join(query_df, filtered_db$taxa)
	
	} else if (!is.null(db_keys)) { # using db_keys

		annotated_db <- as.data.frame(filtered_db$taxa)

	} else { # sanity check -- should not enter here

		stop("issue: no query_df or db_keys") 
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
 #' @param db_keys (Optional) vector of database Keys of entries to include in
 #'   metagenomeAnnotation class object
 #' @param query_df (Optional) data frame with experimental data to annotate with
 #'   taxonomic information, must include column named "Key" with databse ids.
 #' @param ... additional arguments passed to select function
 #' @return mgFeatures-class object
 #' @note Must include either db_keys or query_df as argument.
 #' @rdname annotateFeatures-MgDb-method
 setGeneric("annotateFeatures", signature = "mgdb",
            function(mgdb, ...) {
                standardGeneric("annotateFeatures")}
 )
 #' @export
 #' @examples
 #' # see vignette
 #' @aliases annotate,MgDb-method
 #' @rdname annotateFeatures-MgDb-method
 setMethod("annotateFeatures", "MgDb",
           function(mgdb, db_keys = NULL, query_df = NULL){
               .mgDb_annotateFeatures(mgdb, db_keys,
                              query_df)}
 )