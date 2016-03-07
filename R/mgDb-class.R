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

setOldClass(c("tbl_sqlite"))
#' Metagenome Database class
#'
#' The MgDb-class object contains sequence and taxonomic data for a 16S rRNA
#' taxonomic database, see the \pkg{greengenes13.5MgDb} package as an example
#' database. The \code{get_demoMgDb} function exports a small subset of the database in \pkg{greengenes13.5MgDb}\pkg{metagenomeFeatures} package as an example of a MgDb-class object.
#' @aliases mgdb
#' @field taxa taxonomic information for database sequences
#' @field seq database reference sequences
#' @field metadata associated metadata for the database
#' @export
#' @usage # library(greengenes13.5MgDb)
#' @examples
#' # example MgDb-class object, a small subset of the Greengenes 13.5 database.
#' get_demoMgDb()
#' @note Currently the only database with a MgDb package is the
#'   \href{http://greengenes.secondgenome.com/}{Greengenes database} (version
#'   13.5), additional packages are planned.
#' @rdname MgDb-class
#' @importFrom dplyr tbl_sql
MgDb <- setRefClass("MgDb",
                     #contains="DNAStringSet",
                     fields=list(seq="DNAStringSet",
                                 taxa = "tbl_sqlite",
                                 taxa_file = "character",
                                 metadata= "list"),
                     methods=list(
                         initialize=function(...){
                             callSuper(...)
                             ## TODO - add if statement for passing either a file name or tbl_db
                             taxa_db <- .load_taxa_db(taxa_file)
                             taxa <<- taxa_db
                             seq <<- seq
                             metadata <<- metadata
                         }))

## MgDb Validity ---------------------------------------------------------------
setValidity("MgDb", function(object) {
    msg <- NULL
    if(!("seq" %in% ls(object)) || !is(object$seq, "DNAStringSet"))
        msg <- paste(msg,
                     "'seq' slot must contain DNAStringSeq object",
                     sep = "\n")
    if(!("taxa" %in% ls(object)) || !is(object$taxa, "tbl_sqlite"))
        msg <- paste(msg,
                     "'taxa' slot must contain a tbl object",
                     sep = "\n")
    if(!("metadata" %in% ls(object)) || !is(object$metadata, "list"))
        msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
    if (is.null(msg)) TRUE else msg
})

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

#' Display summary of MgDb-class object
#' @param object MgDb-class object

#' @export
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

#' Querying MgDb objects
#'
#' Function for querying \code{\link{MgDb}} class objects, user defines the
#' taxonomic levels (\code{keytype}) and a vector of taxonomic names
#' (\code{keys}) being selected. If specific database ids are being selected for
#' use \code{keytype="Keys"}. Additionally, users can specify whether they want
#' only the taxonomic and sequence data, or both.
#'
#' @param mgdb MgDb class object
#' @param type either "taxa", "seq", or "both". "taxa" and "seq" only queries
#'   the taxonomy and sequences databases respectively. "both" queries both the
#'   taxonomy and sequence database.
#' @param keys specific taxonomic groups to select for
#' @param keytype taxonomic level of keys
#' @param columns keytypes in taxonomy databse to return, all by default
#' @param ... additional arguments passed to select function
#' @return returned object depends on type, for 'taxa' -dataframe with taxa
#'   information, 'seq' a DNAStringSet with seqeunce data, 'both' a list with
#'   the dataframe and DNAStringSet.
#' @examples
#' demoMgDb <- get_demoMgDb()
#' # select taxa only
#' select(demoMgDb, type = "taxa",
#'      keys = c("Vibrio", "Salmonella"),
#'      keytype = "Genus")
#'
#' # select seq only
#'  select(demoMgDb, type = "seq",
#'       keys = c("Vibrio", "Salmonella"),
#'       keytype = "Genus")
#'
#' # select both taxa and seq
#' select(demoMgDb, type = "both",
#'        keys = c("Vibrio", "Salmonella"),
#'        keytype = "Genus")
#' @rdname select-MgDb-method
setGeneric("select", signature="mgdb",
    function(mgdb, type, ...) { standardGeneric("select")
})


#' @export
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
## returns a metagenomeAnnotation class object query - either a vector of
## database Keys, data frame with a column of database Keys and sample count
## data, a DNAStringSet with user provided sequences, or a vector with all three
## or a data frame with database Keys, query seq ids, can also include count
## data potentially modify to accept a fasta/ fastq file mapping - defines
## method to map user provided sequences to database arbitrary - for
## developmental use only randomly selects random subset of sequences in the
## database to assign as matching sequences to the first 100 sequences in the
## query set



.mgDb_annotate <- function(mgdb, db_keys, query_df = NULL,
                           query_seq = NULL, mapping = NULL){
    if(is.null(db_keys)){
        if(is.null(query_df)){
            stop("must provide either 'db_keys' or 'query_df'")
        }else if("Keys" %in% colnames(query_df)){
            select_keys <- as.character(query_df$Keys)
        }else{
            stop("Need 'Keys' column 'query_df' with database ids")
        }
    }else{
        message("Using 'db_keys' for subset database")
        select_keys <- as.character(db_keys)
    }

    filtered_db <- select(mgdb, type = "both",
                          keys = select_keys,
                          keytype = "Keys")
    if(is.null(query_df)){
        annotated_db <- as.data.frame(filtered_db$taxa)
    }else{
        query_df$Keys <- as.character(query_df$Keys)
        annotated_db <- dplyr::right_join(query_df, filtered_db$taxa)
    }

    if(is.null(query_seq)){
        exp_seq_data <- new("DNAStringSet")
    }else if(is(query_seq, "DNAStringSet")){
            exp_seq_data <- query_seq
    }else{
        warning(paste0("query_seq is not a 'DNAStringSet' class object,",
                        "not including in the metagenomeAnnotation object"))
        exp_seq_data <- new("DNAStringSet")
    }

    anno_metadata <- mgdb$metadata
    anno_metadata$mapping <- mapping

    new("metagenomeAnnotation",
        data = annotated_db,
        metadata = anno_metadata,
        experimentSeqData = exp_seq_data
    )

}

### ============================================================================
##
##                              MgDb annotate MRexperiment
##
### ============================================================================

#' Annotating metagenome data with taxonomic information
#'
#' This method is used to create a \linkS4class{metagenomeAnnotation} class
#' object with user supplied taxonomic assignments and \link[=MgDb]{MgDb-class}
#' object. As input users can provide a vector with database ids, a data.frame
#' with database ids as well as count data for different samples as columns
#' along with a column of database ids named \code{Keys}, additionally a
#' \code{\link[Biostrings]{DNAStringSet}} object can be passed with experimental
#' sequence data.  If experimental sequence data are provided, database ids must
#' be passed as a data.frame and include a column \code{SeqIDs} with sequence
#' names as well as database ids.
#'
#' @param mgdb MgDb class object
#' @param db_keys (Optional) vector of database Keys of entries to include in
#'   metagenomeAnnotation class object
#' @param query_df (Optional) data frame with experimental data to annotate with
#'   taxonomic information, must include column named "Key" with databse ids.
#' @param query_seq (Optional) DNAStringSet object sequences
#' @param mapping (Optional) method used to map sequences to database, default
#'   "user provided", use for documenting methods used to perfom the taxonomic
#'   assignment.
#' @param ... additional arguments passed to select function
#' @return metagenomeAnnotation-class object
#' @note Must include either db_keys or query_df as argument.
#' @rdname annotate-MgDb-method
setGeneric("annotate", signature = "mgdb",
           function(mgdb, ...) {
               standardGeneric("annotate")}
)

#' @export
#' @examples
#' # see vignette
#' @aliases annotate,MgDb-method
#' @rdname annotate-MgDb-method
setMethod("annotate", "MgDb",
          function(mgdb, db_keys = NULL, query_df = NULL,
                   query_seq = NULL, mapping = "user provided ids"){
              .mgDb_annotate(mgdb, db_keys,
                             query_df, query_seq, mapping)}
)

#' Annotate MRexperiment object with seq taxonomy from MgDb object
#'
#' This method is used annotate a MRexperiment with taxonomic information from a \link[=MgDb]{MgDb-class}
#' object using the MRexperiment object's Feature names.
#' object.
#'
#' @param mgdb MgDb class object
#' @param MRobj MRexperiment class object
#' @param ... additional arguments passed to select function
#' @return metagenomeAnnotation-class object
#' @note Must include either db_keys or query_df as argument.
#' @rdname annotate-MgDb-method
setGeneric("annotateMRexp", signature = "mgdb",
           function(mgdb, MRobj, ...) {
               standardGeneric("annotateMRexp")}
)

#' Annotate MRexperiment object with seq taxonomy from MgDb object
#'
#' This method is used annotate a MRexperiment with taxonomic information from a \link[=MgDb]{MgDb-class}
#' object using the MRexperiment object's Feature names.
#' object.
#'
#' @param mgdb MgDb class object
#' @param MRobj MRexperiment class object
#' @param ... additional arguments passed to select function
#' @return metagenomeAnnotation-class object
#' @export
#' @examples
#' # see vignette
#' @aliases annotateMRexp,MgDb-method
#' @rdname annotateMRexp-MgDb-method
setMethod("annotateMRexp", "MgDb",
          function(mgdb, MRobj){
    db_keys <- featureNames(MRobj)
    anno <- .mgDb_annotate(mgdb,db_keys,
                           query_df = NULL,
                           query_seq = NULL,
                           mapping = NULL)
    rownames(anno@data) <- anno@data$Keys
    featureData(MRobj) <- anno
    MRobj
}
)
