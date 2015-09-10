## -----------------------------------------------------------------------------
##
## Definition and methods for MgDb class
## using RefClass as db changes state with query string
##
## -----------------------------------------------------------------------------

## Not sure how to access method, currently need MgDb$methodname(mgdb_object)
#loading the sqlite database from file
.load_taxa_db <- function(taxdb){
    db_con <- dplyr::src_sqlite(taxdb)
    dplyr::tbl(src = db_con, from = "taxa")
}

#' Metagenome Database class
#' @aliases mgdb
#' @field taxa taxonomic information for database sequences
#' @field seq database reference sequences
#' @field metadata associated metadata for the database
#' @export
#' @rdname MgDb-class
MgDb <- setRefClass("MgDb",
                     contains="DNAStringSet",
                     fields=list(seq="DNAStringSet",
                                 taxa = "ANY",
                                 metadata= "list"),
                     methods=list(
                         initialize=function(...){
                             callSuper(...)
                             taxa <<- .load_taxa_db(taxa)
                             seq <<- seq
                             metadata <<- metadata
                         }))

### Metadata - Need to change namespace to not export - Not exported.
# DB_TYPE_NAME <- "Db type"
# DB_TYPE_VALUE <- "MgDb"  # same as the name of the class
# DB_SCHEMA_VERSION <- "1.0"

## MgDb Validity ---------------------------------------------------------------
## not sure how to set validity for refClass
# setValidity("MgDb", function() {
#     msg <- NULL
#     if(!("seq" %in% ls(.self)) || !is(.self@seq, "DNAStringSet"))
#         msg <- paste(msg, "'seq' slot must contain a DNAStringSeq object with sequence data", sep = "\n")
#     if(!("taxa" %in% ls(.self)) || !is(.self@taxa, "tbl_sqlite"))
#         msg <- paste(msg, "'taxa' slot must contain a tbl_sqlite object with taxonomy data", sep = "\n")
#     if(!("metadata" %in% ls(.self)) || !is(.self@metadata, "list"))
#         msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
#     if (is.null(msg)) TRUE else msg
# })

################################################################################
################################################################################
##
##                              MgDb Methods
##
################################################################################
################################################################################

### ============================================================================
##
##                              MgDb show method
##
### ============================================================================
## Need to revise for refClass structure

#' Display summary of MgDb-class object
#' @param object MgDb-class object
#'
#' @export
#' @rdname MgDb-class
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
        }
)

### ============================================================================
##
##                              MgDb select method
##
### ============================================================================


.select.seq <- function(seqObj, ids){
    seqObj[names(seqObj) %in% ids,]
}


.select.taxa<- function(taxaDb, keys, keytype,
                        columns="all"){
    # selecting desired rows
    if(keytype !=  "Keys"){
        level_id <- rep(tolower(stringr::str_sub(string = keytype,
                                             start = 1,end = 1)), length(keys))
        keys <- stringr::str_c(level_id,keys,sep = "__")
    }
    if(length(keys) == 1){
        filter_criteria <- lazyeval::interp(~which_column == keys,
                                            which_column = as.name(keytype))
    } else {
        filter_criteria <- lazyeval::interp(~which_column %in% keys,
                                            which_column = as.name(keytype))
    }

    select_tbl <- dplyr::filter_(taxaDb, filter_criteria)

    # selecting desired columns
    if(columns[1] != "all"){
        select_tbl <- dplyr::select_(select_tbl, .dots = columns)
    }

    return(select_tbl %>% dplyr::collect())
}

# select function returns a filtered set of sequences or taxa based on defined
# input ' use type = "seq" returns a ShortRead object and type = "taxa" returns a
# filtered database not sure if we want to make the select method only generate a

## either select by ids for taxa information
.select <- function(mgdb, type, keys, keytype, columns){
    if(!(type %in% c("seq","taxa", "both"))){
        stop("type must be either 'seq', 'taxa', or both")
    }

    if(is.null(keys) + is.null(keytype) == 1){
        stop("must define both keys and keytypes, or neither")
    }
    taxa_df <- .select.taxa(mgdb$taxa, keys, keytype, columns)
    if(type == "taxa"){
        return(taxa_df)
    }


    if(type == "seq" || type == "both"){
        seq_obj <- .select.seq(mgdb$seq, taxa_df$Keys)
        if(type != "both"){
            return(seq_obj)
        }
    }

    return(list(taxa = taxa_df, seq = seq_obj))
}

#' Function for querying MgDb class objects
#'  Use keys - keytype sets to define subset.
#'  keytype - is the taxonomic level with the taxa of interest, use "Keys" if selecting based on selecting based on sequence IDs.
#'  keys - is a vector of values of keytype
#' @param mgdb MgDb class object
#' @param type either "taxa", "seq", or "both". "taxa" and "seq" only queries the taxonomy and sequences databases respectively. "both" queries both the taxonomy and sequence database.
#' @param keys specific taxonomic groups to select for
#' @param keytype taxonomic level of keys
#' @param columns keytypes in taxonomy databse to return, all by default
#' @param ... additional arguments passed to select function
#' @return generates database, function does not return anything
#' @export
#' @rdname select-MgDb-method
setGeneric("select", signature="mgdb",
    function(mgdb, type, ...) { standardGeneric("select")
})



#' @aliases select,MgDb-method
#' @rdname select-MgDb-method
setMethod("select", "MgDb",
          function(mgdb, type, keys = NULL, keytype = NULL, columns = "all"){
              .select(mgdb, type, keys, keytype, columns)
          }
)


### ============================================================================
##
##                              MgDb annotate method
##
### ============================================================================

## Methods to generate metagenomeAnnotation object %%TODO%% modify features to
## include additional information about metagenomeAnnotation class, specifically
## filter command and other approriate metadata, e.g. method used for mapping
## returns a metagenomeAnnotation class object
## query - either a vector of database Keys, data frame with a column of database Keys and sample count data, a DNAStringSet with user provided sequences, or a vector with all three or a data frame with database Keys, query seq ids, can also include count data
##      potentially modify to accept a fasta/ fastq file
## mapping - defines method to map user provided sequences to database
##              arbitrary - for developmental use only randomly selects random subset of sequences in the database to assign as matching sequences to the first 100 sequences in the query set


##%%%###%%%
##%%% TODO
## 1. add tests
## 2. update documentation
## 3. adding db seq slot to metagenomeFeatures class definition
.mgDb_annotate <- function(mgdb, db_keys, query_df, query_seq, mapping){#mapping = "arbitrary"){
#     if(mapping == "arbitrary"){
#         warning("Arbitrary mapping method is for development purposes, mappings are to the first entries in the database and not intended to represent actual sequence taxonomic assignment")
#         query_size <- length(query)
#         keys <- taxa_keys(mgdb, keytype = c("Keys"))$Keys
#         key_subset <- keys[1:query_size]
#         match_df <- data.frame(query_id = names(ShortRead::sread(query)),
#                                Keys = key_subset,
#                                stringsAsFactors = FALSE)
#     }else{
#         stop("Only arbirary mapping method is currently implemented")
#     }
#
    if(is.null(db_keys)){
        if(is.null(query_df)){
            stop("must provide either 'db_keys' or 'query_df'")
        }else{
            if("Keys" %in% colnames(query_df)){
                select_keys <- query_df$Keys
            }else{
                stop("Need column in 'query_df' with database seq ids with name 'Keys'")
            }
        }
    }else{
        message("Using 'db_keys' for subset database")
        select_keys <- db_keys
    }

    filtered_db <- select(mgdb, type = "both",
                          keys = select_keys,
                          keytype = "Keys")
    if(is.null(query_df)){
        annotated_db <- filtered_db$taxa
    }else{
        annotated_db <- dplyr::right_join(query_df, filtered_db$taxa)
    }

    if(is.null(query_seq)){
        exp_seq_data <- new("DNAStringSet")
    }else{
        if(is(query_seq, "DNAStringSet")){
            exp_seq_data <- query_seq
        }
        warning("query_seq is not a 'DNAStringSet' class object, not including in the metagenomeAnnotation object" )
        exp_seq_data <- new("DNAStringSet")
    }

    anno_metadata <- mgdb$metadata
    anno_metadata$mapping <- mapping

    new("metagenomeAnnotation",
        annotation_data = new("AnnotatedDataFrame", annotated_db),
        metadata = anno_metadata,
        feature_data = exp_seq_data
    )

}

#' annotating a set of sequences with taxonomic information from a MgDb class object
#' @param mgdb MgDb class object
#' @param db_keys (Optional) vector of database Keys of entries to include in metagenomeAnnotation class object
#' @param query_df (Optional) data frame with experimental data to annotate with taxonomic information, must include column named "Key" with data base Keys of entries to include in the new metagenomeAnnotation class object
#' @param query_seq (Optional) DNAStringSet object with experiment marker gene sequences
#' @param mapping (Optional) method used to map sequences to database, user provided
#' @param ... additional arguments passed to select function
#' @return metagenomeAnnotation class object
#' @note Must include either db_keys or query_df arguments
#' @export
#' @rdname annotate-MgDb-method
setGeneric("annotate", signature = "mgdb",
           function(mgdb, ...) {
               standardGeneric("annotate")}
)

#' @aliases annotate,MgDb-method
#' @rdname annotate-MgDb-method
setMethod("annotate", "MgDb",
          function(mgdb, db_keys = NULL, query_df = NULL,
                   query_seq = NULL, mapping = "user provided ids"){
              .mgDb_annotate(mgdb, db_keys,
                             query_df, query_seq, mapping)}
)
