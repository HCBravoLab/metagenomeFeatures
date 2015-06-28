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
#' @name MgDb
#' @import methods
#' @exportClass MgDb
#' @field taxa taxonomy database
#' @field seq database reference sequences
#' @field metadata database metadata
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
setMethod("show", "MgDb",
          function(object){
            cat(class(object), "object:\n")
            print("Metadata\n")
            metadata <- object $metadata
                for(i in names(metadata)){
                    cat("|", i, ": ", metadata[[i]], "\n", sep = "")
                }
            print("Sequence Data:\n")
            print(object$seq)
            print("Taxonomy Data:\n")
            print(object$taxa)
        }
)

### ============================================================================
##
##                              MgDb select method
##
### ============================================================================

## Methods to generate metagenomeAnnotation object %%TODO%% modify features to
## include additional information about metagenomeAnnotation class, specifically
## filter command and other approriate metadata, e.g. method used for mapping
## returns a metagenomeAnnotation class object
## query - is a DNAStringSet with user provided sequences
##      potentially modify to accept a fasta/ fastq file
## mapping - defines method to map user provided sequences to database
##              arbitrary - for developmental use only randomly selects random subset of sequences in the database to assign as matching sequences to the first 100 sequences in the query set

MgDb$methods(
    annotate = function(query, mapping = "arbitrary",...){
        if(mapping == "arbitrary"){
            query_size <- length(query)
            db_subset <- sample(.self$taxa_keys(keytype = c("Keys"))$Keys,query_size)
            match_df <- data.frame(query_id = names(query_subset),
                                   Keys = db_subset,
                                   stringsAsFactors = FALSE)
        }
        filtered_db <- .self$select(type = "both",
                                     keys = match_df$Keys,
                                     keytype = "Keys")
#
        annotated_db <- dplyr::right_join(match_df, filtered_db$taxa)
        anno_metadata <- .self$metadata
        anno_metadata$mapping <- mapping
#
        new("metagenomeAnnotation",
            refDF = annotated_db,
            metadata = anno_metadata,
            feature_data = query
        )

    }
)


.select.seq <- function(seqObj, ids, ...){
    seqObj[names(seqObj) %in% ids,]
}

#' Function for querying the marker gene taxonomy database.
#'
#' @param ids sequence ids to select
#'
#' @param columns quoted vector of table columns returned, all returned by default
#'
#' @param sqlite database connection, see src_sqlite in dplyr
#'
#' @param dbtable database table name, defaults to tree.
#'
#' @return generates database, function does not return anything
.select.taxa<- function(taxaDb, keys, keytype,
                        columns="all"){

    # selecting desired rows
    if(keytype !=  "Keys"){
        level_id <- tolower(stringr::str_sub(string = keytype,
                                             start = 1,end = 1))
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
MgDb$methods(select = function(type, ids = NULL, ...){
                if(!(type %in% c("seq","taxa", "both"))){
                    stop("type must be either 'seq' or 'taxa'")
                }

                if(type == "taxa"|| type == "both" || is.null(ids)){
                    taxa_df <- .select.taxa(.self$taxa, ...)
                    if(type == "taxa"){
                        return(taxa_df)
                    }
                    if(is.null(ids)){
                            ids <- taxa_df$Keys
                    }
                }

                if(type == "seq" || type == "both"){
                    seq_obj <- .select.seq(.self$seq, ids, ...)
                    if(type != "both"){
                        return(seq_obj)
                    }
              }

              return(list(taxa = taxa_df, seq = seq_obj))
          }
)


### ============================================================================
##
##                              MgDb Taxa methods
##
### ============================================================================

## See post for functional programming interface for refClass methods http://stackoverflow.com/questions/20649973/functional-interfaces-for-reference-classes
### Not sure how to best document and export MgDb methods, wrote wrappers
### so they can be used using standard function(value) method

### Taxa keys function ---------------------------------------------------------
MgDb$methods(taxa_keys = function(keytype){
                dplyr::select_(.self$taxa, keytype) %>% dplyr::collect()
          }
)

## Wrapper for taxa_keys method

#' Taxonomy values for a given keytype
#'
#' @param mgdb_object object of MgDB class
#' @param keytype taxonomic classification level
#'
#' @return tbl_df
#' @export
#'
#' @examples taxa_keys(mgdb, "Class")
taxa_keys <- function(mgdb_object, keytype){
    return(mgdb_object$taxa_keys(keytype))
}


### Taxa columns function ------------------------------------------------------
MgDb$methods(taxa_columns = function(){
        colnames(.self$taxa)
    }
)

## Wrapper for taxa_columns method

#' Taxonomic levels for a MdDB class
#'
#' @param mgdb_object object of MgDB class
#' @param keytype taxonomic classification level
#'
#' @return vector
#' @export
#'
#' @examples taxa_keys(mgdb)
taxa_columns <- function(mgdb_object){
    return(mgdb_object$taxa_columns(mgdb_object))
}

### taxa keytypes function -----------------------------------------------------
## %%TODO%% have return an AnnotatedDataFrame
MgDb$methods(taxa_keytypes = function(){
        colnames(.self$taxa)
    }
)

## Wrapper for taxa_keytypes method

#' Keytypes for the MgDB object
#'
#' @param mgdb_object object of MgDB class
#'
#' @return vector
#' @export
#'
#' @examples taxa_keytypes(mgdb)
taxa_keytypes <- function(mgdb_object){
    return(mgdb_object$taxa_keytypes())
}
