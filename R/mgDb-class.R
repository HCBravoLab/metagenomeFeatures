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
    dplyr::tbl(src = db_con, from = "tree")
}

#' Metagenome Database class
#'
#' @field taxa taxonomy database
#' @field seq database reference sequences
#' @field metadata database metadata
MgDb <- setRefClass("MgDb",
                     contains="ShortRead",
                     fields=list(seq="ShortRead",
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
setValidity("MgDb", function(object) {
    msg <- NULL
    if(!("seq" %in% ls(object)) || !is(object@seq, "ShortRead"))
        msg <- paste(msg, "'seq' slot must contain a ShortRead object with sequence data", sep = "\n")
    if(!("taxa" %in% ls(object)) || !is(object@taxa, "tbl_sqlite"))
        msg <- paste(msg, "'taxa' slot must contain a tbl_sqlite object with taxonomy data", sep = "\n")
    if(!("metadata" %in% ls(object)) || !is(object@metadata, "list"))
        msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
    if (is.null(msg)) TRUE else msg
})

## MgDb Methods ----------------------------------------------------------------
## General Methods
setMethod("show", "MgDb",
          function(object){
            cat(class(object), "object:\n")
            print("Metadata\n")
            metadata <-object$metadata
                for(i in names(metadata)){
                    cat("|", i, ": ", metadata[[i]], "\n", sep = "")
                }
            print("Sequence Data:\n")
            print(object$seq)
            print("Taxonomy Data:\n")
            print(object$taxa)
        }
)

##
##
## Methods to generate metagenomeAnnotation object %%TODO%% modify features to
## include additional information about metagenomeAnnotation class, specifically
## filter command and other approriate metadata, e.g. method used for mapping
MgDb$methods(annotate = function(object, ...){
                filtered_db <- select(object, type = "both", ...)
                MgDb$new(taxa = filtered_db[[1]],
                          seq = filtered_db[[2]],
                          features = object$metadata)
            }
)

## select methods --------------------------------------------------------------
## use ShortRead Object filters
.select.seq <- function(x, ...){
    return
}

#' Function for querying the marker gene taxonomy database.
#' @param ids sequence ids to select
#' @param columns quoted vector of table columns returned, all returned by default
#' @param sqlite database connection, see src_sqlite in dplyr
#' @param dbtable database table name, defaults to tree.
#' @return generates database, function does not return anything
.select.taxa<- function(keys, keytype,
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
MgDb$methods(select = function(object, type, ...){
              if(!(type %in% c("seq","taxa", "both"))){
                  stop("type must be either 'seq' or 'taxa'")
              }
              if(type == "seq" || type == "both"){
                  seq_obj <- .select.seq(object$seq, ...)
                  if(type != "both"){
                      return(seq_obj)
                  }
              }
              if(type == "taxa"|| type == "both"){
                  taxa_df <- .select.taxa(object$taxa, ...)
                  if(type != "both"){
                      return(taxa_obj)
                  }
              }else{
                  taxa_df <- object$taxa
              }
              return(list(taxa_df, seq_df))
          }
)

## MgDb Taxa function ----------------------------------------------------------


#' Retrieve key values for a specific taxa
#' @name taxa_keys
#' @param object
#' @param keytype
#'
#' @return tbl_df
#' @export
#'
#' @examples TODO
MgDb$methods(taxa_keys = function(object, keytype){
                dplyr::select_(object$taxa, keytype) %>% dplyr::collect()
          }
)

#' List of database columns.
#' @name taxa_columns
#' @param object
#'
#' @return vector
#' @export
#'
#' @examples TODO
MgDb$methods(taxa_columns = function(object){
        colnames(object$taxa)
    }
)

#' Field to potentially use to query database.
#' @name taxa_keytypes
#' @param object
#'
#' @return vector
#' @export
#'
#' @examples TODO
MgDb$methods(taxa_keytypes = function(object){
        colnames(object$taxa)
    }
)
