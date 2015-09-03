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
#'
#' @field taxa taxonomic information for database sequences
#' @field seq database reference sequences
#' @field metadata associated metadata for the database
#' @exportClass
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

#' @exportMethod
setMethod("show", "MgDb",
          function(object){
            cat(class(object), "object:")
            print("Metadata\n")
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
.select <- function(x, type, keys, keytype, columns = "all", ids = NULL){
    if(!(type %in% c("seq","taxa", "both"))){
        stop("type must be either 'seq' or 'taxa'")
    }

    if(type == "taxa"|| type == "both" || is.null(ids)){
        taxa_df <- .select.taxa(x$taxa, keys, keytype, columns)
        if(type == "taxa"){
            return(taxa_df)
        }
        if(is.null(ids)){
            ids <- taxa_df$Keys
        }
    }

    if(type == "seq" || type == "both"){
        seq_obj <- .select.seq(x$seq, ids)
        if(type != "both"){
            return(seq_obj)
        }
    }

    return(list(taxa = taxa_df, seq = seq_obj))
}

setGeneric("select", function(x, type, keys, keytype, ids = NULL, ...) {
    standardGeneric("select")
})

#' Function for querying MgDb class objects
#' @param x MgDb class object
#'
#' @param type either "taxa", "seq", or "both". "taxa" and "seq" only queries the taxonomy and sequences databases respectively. "both" queries both the taxonomy and sequence database.
#' @param keys specific taxonomic groups to select for
#' @param keytype taxonomic level of keys
#' @param columns keytypes in taxonomy databse to return, all by default
#' @param ids sequence ids to select
#' @return generates database, function does not return anything
#' @exportMethod
setMethod("select", "MgDb",
          function(x, type, keys, keytype, ids = NULL, ...){
              .select(x, type, keys, keytype, ids = NULL, ...)
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
## query - is a DNAStringSet with user provided sequences
##      potentially modify to accept a fasta/ fastq file
## mapping - defines method to map user provided sequences to database
##              arbitrary - for developmental use only randomly selects random subset of sequences in the database to assign as matching sequences to the first 100 sequences in the query set

.mgDb_annotate <- function(x, query, mapping = "arbitrary",...){
    if(mapping == "arbitrary"){
        warning("Arbitrary mapping method is for development purposes, mappings are to the first entries in the database and not intended to represent actual sequence taxonomic assignment")
        query_size <- length(query)
        keys <- taxa_keys(x, keytype = c("Keys"))$Keys
        key_subset <- keys[1:query_size]
        match_df <- data.frame(query_id = names(sread(query)),
                               Keys = key_subset,
                               stringsAsFactors = FALSE)
    }else{
        stop("Only arbirary mapping method is currently implemented")
    }
    filtered_db <- select(x, type = "both",
                          keys = match_df$Keys,
                          keytype = "Keys")

    annotated_db <- dplyr::right_join(match_df, filtered_db$taxa)
    anno_metadata <- x$metadata
    anno_metadata$mapping <- mapping

    new("metagenomeAnnotation",
        refDF = annotated_db,
        metadata = anno_metadata,
        feature_data = sread(query)
    )

}

setGeneric("annotate",
           function(x, ...) {standardGeneric("annotate")}
)

#' annotating a set of sequences with taxonomic information from a MgDb class object
#' @param x MgDb class object
#' @param query ShortRead-class object with marker gene sequences
#' @param mapping method used to map sequences to database
#' @return metagenomeAnnotation class object
#' @exportMethod
setMethod("annotate", "MgDb",
          function(x, query, mapping, ...){ .mgDb_annotate(x, query, mapping, ...)}
)
