################################################################################
##
## Definition and methods for MgDb class
## using RefClass as db changes state with query string
##
################################################################################


## MgDb Class -----------------------------------------------------------------------------

## Not sure how to access method, currently need MgDb$methodname(mgdb_object)
## loading the sqlite database from file

.load_taxa_db <- function(taxdb){
    db_con <- dplyr::src_sqlite(taxdb)
    dplyr::tbl(src = db_con, from = "taxa")
}

.load_tree <- function(tree_file){
    if(grepl("rds",tree_file)){
        tree <- readRDS(tree_file)
    }else{
       tree <- ape::read.tree(tree_file)
    }
    tree
}

setOldClass(c("tbl_sqlite"))
setOldClass("phylo")
#' Metagenome Database class
#'
#' The MgDb-class object contains sequence and taxonomic data for a 16S rRNA
#' taxonomic database, see the \pkg{greengenes13.5MgDb} package as an example
#' database. The \code{get_demoMgDb} function exports a small subset of the database in \pkg{greengenes13.5MgDb}\pkg{metagenomeFeatures} package as an example of a MgDb-class object.
#' @aliases mgdb
#' @field taxa taxonomic information for database sequences
#' @field seq database reference sequences
#' @field tree reference phylogenetic tree
#' @field taxa_file name of sqlite db
#' @field tree_file name of phylogenetic tree file
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
                     #contains="DNAStringSet"
                     fields=list(seq="DNAStringSet",
                                 # add seq file inplace of reading DNAStringSet
                                 taxa = "tbl_sqlite",
                                 taxa_file = "character",
                                 tree_file = "character",
                                 tree = "phylo",
                                 metadata= "list"),
                     methods=list(
                         initialize=function(...){
                             callSuper(...)
                             taxa <<- .load_taxa_db(taxa_file)
                             seq <<- seq
                             if(tree_file != "not available"){
                                 tree <<- .load_tree(tree_file)
                             }
                             metadata <<- metadata
                         }))

## Validity ---------------------------------------------------------------

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
    if(!("tree" %in% ls(object)) || !is(object$tree, "phylo"))
        msg <- paste(msg,
                     "'tree' slot must contain a phylo object",
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

## Show ------------------------------------------------------------------------

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
            print("Tree Data:")
            print(object$tree)
        }
)

## Accessors -------------------------------------------------------------------

#' MgDb tree slot accessor
#'
#' @param mgdb  MgDb class object
#'
#' @return phylo class object
#' @export
#'
#' @examples # mgdb_tree(demoMgDb)
mgdb_tree <- function(mgdb){
    mgdb$tree
}

#' MgDb seq slot accessor
#'
#' @param mgdb  MgDb class object
#'
#' @return DNAStringSet class object
#' @export
#'
#' @examples # mgdb_seq(demoMgDb)
mgdb_seq <- function(mgdb){
    mgdb$seq
}

#' MgDb taxa slot accessor
#'
#' @param mgdb  MgDb class object
#'
#' @return tbl_sql connection to sqlite table
#' @export
#'
#' @examples # mgdb_taxa(demoMgDb)
mgdb_taxa <- function(mgdb){
    mgdb$taxa
}

#' MgDb metadata slot accessor
#'
#' @param mgdb  MgDb class object
#'
#' @return list
#' @export
#'
#' @examples # mgdb_meta(demoMgDb)
mgdb_meta <- function(mgdb){
    mgdb$metadata
}

################################################################################
##
##                              MgDb select method
##
################################################################################
## Select ----------------------------------------------------------------------

.select.seq <- function(seqObj, ids){
    seqObj[names(seqObj) %in% ids,]
}


.select.taxa<- function(taxaDb, metaDb, keys, keytype,
                        columns="all"){

    ## setting values when keys and keytypes are not defined
    if(is.null(keys)){
        keys <- taxaDb %>% dplyr::select(Keys) %>%
            dplyr::collect() %>% .$Keys
        keytype <- "Keys"
    }

    # selecting desired rows


    if(!is.null(keys)){
        if(keytype !=  "Keys"){
            level_id <- stringr::str_sub(string = keytype, start = 1,end = 1) %>%
                tolower() %>% rep(length(keys))
            if(metaDb$DB_TYPE_NAME =="GreenGenes"){
                keys <- stringr::str_c(level_id,keys,sep = "__")
            }
        }

        if(length(keys) == 1){
            filter_criteria <- lazyeval::interp(~which_column == keys,
                                                which_column = as.name(keytype))
        } else {
            filter_criteria <- lazyeval::interp(~which_column %in% keys,
                                                which_column = as.name(keytype))
        }
        select_tbl <- dplyr::filter_(taxaDb, filter_criteria)
    }else{
        select_tbl <- taxaDb
    }


    # selecting desired columns
    if(columns[1] != "all"){
        select_tbl <- dplyr::select_(select_tbl, .dots = columns)
    }

    return(select_tbl %>% dplyr::collect())
}

.select.tree <- function(tree, ids){
    drop_tips <- tree$tip.label[!(tree$tip.label %in% ids)]
    ape::drop.tip(tree,drop_tips)
}

## either select by ids for taxa information
.select <- function(mgdb, type, keys, keytype, columns){
    ## check correct types
    select_types <- c("seq","taxa", "tree", "all")
    if(FALSE %in% (type %in% select_types)){
        bad_type <- type[!(type %in% select_types)]
        stop(paste(bad_type, "not valid type value, type must be either 'seq', 'taxa', 'tree', 'all' or a character vector with types"))
    }

    if(is.null(keys) != is.null(keytype)){
        stop("must define both keys and keytypes, or neither")
    }

    ## list with select results
    select_obj <- list()
    taxa_df <- .select.taxa(mgdb$taxa, mgdb$metadata, keys, keytype, columns)


    # vector with all objects to return
    if("taxa" %in% type || type == "all"){
        select_obj$taxa <- taxa_df
    }

    if("seq" %in% type || type == "all"){
        select_obj$seq <- .select.seq(mgdb$seq, taxa_df$Keys)
    }

    if("tree" %in% type || type == "all"){
        select_obj$tree <- .select.tree(mgdb$tree, taxa_df$Keys)
    }

    ## return single obj if only selecting one type
    if(type != "all" && length(type)  == 1){
        select_obj <- select_obj[[type]]
    }

    return(select_obj)
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
#' @param type either "taxa", "seq", "tree", "all" or a character vector of types. "taxa", "seq", and "tree" only query
#'   the reference taxonomy, sequences, and phylogenetic tree respectively.
#'   "all" queries the reference taxonomy, sequence, and phylogenetic tree.
#' @param keys specific taxonomic groups to select for
#' @param keytype taxonomic level of keys
#' @param columns keytypes in taxonomy databse to return, all by default
#' @param ... additional arguments passed to select function
#' @return returned object depends on type, for 'taxa' -dataframe with taxa
#'   information, 'seq' a DNAStringSet with seqeunce data, 'tree' a phylogenetic tree of class phylo, 'all' a list with
#'   the dataframe, DNAStringSet, and phylo.
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
#' # select all taxa, seq, and tree
#' select(demoMgDb, type = "all",
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


################################################################################
##
##                              MgDb annotate method
##
################################################################################
## Annotate --------------------------------------------------------------------

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



# .mgDb_annotate <- function(mgdb, db_keys,
#                            query_df = NULL, exp_seq = NULL, exp_tree = NULL,
#                            mapping = NULL){
#     if(is.null(db_keys)){
#         if(is.null(query_df)){
#             stop("must provide either 'db_keys' or 'query_df'")
#         }else if("Keys" %in% colnames(query_df)){
#             select_keys <- as.character(query_df$Keys)
#         }else{
#             stop("Need 'Keys' column 'query_df' with database ids")
#         }
#     }else{
#         message("Using 'db_keys' for subset database")
#         select_keys <- as.character(db_keys)
#     }
#
#     filtered_db <- select(mgdb, type = "all",
#                           keys = select_keys,
#                           keytype = "Keys")
#     if(is.null(query_df)){
#         annotated_db <- as.data.frame(filtered_db$taxa)
#     }else{
#         query_df$Keys <- as.character(query_df$Keys)
#         annotated_db <- dplyr::right_join(query_df, filtered_db$taxa)
#     }
#
#     if(is.null(exp_seq)){
#         exp_seq_data <- NULL
#     }else if(is(exp_seq, "DNAStringSet")){
#             exp_seq_data <- query_seq
#     }else{
#         warning(paste0("exp_seq is not a 'DNAStringSet' class object,",
#                         "not including in the metagenomeAnnotation object"))
#         exp_seq_data <- NULL
#     }
#
#     if(is.null(exp_tree)){
#         exp_tree_data <- NULL
#     }else if(is(exp_tree, "phylo")){
#         exp_tree_data <- exp_tree
#     }else{
#         warning(paste0("exp_tree is not a 'phylo' class object,",
#                        "not including in the metagenomeAnnotation object"))
#         exp_tree_data <- NULL
#     }
#
#     anno_metadata <- mgdb$metadata
#     anno_metadata$mapping <- mapping
#
#     new("metagenomeAnnotation",
#         data = annotated_db,
#         metadata = anno_metadata,
#         referenceDbSeqData=filtered_db$seq,
#         referenceDbTreeData=filtered_db$tree,
#         experimentSeqData = exp_seq_data,
#         experimentTreeData=exp_tree_data
#     )
# }

# #' Annotating metagenome data with taxonomic information
# #'
# #' This method is used to create a \linkS4class{metagenomeAnnotation} class
# #' object with user supplied taxonomic assignments and \link[=MgDb]{MgDb-class}
# #' object. As input users can provide a vector with database ids, a data.frame
# #' with database ids as well as count data for different samples as columns
# #' along with a column of database ids named \code{Keys}, additionally a
# #' \code{\link[Biostrings]{DNAStringSet}} object can be passed with experimental
# #' sequence data.  If experimental sequence data are provided, database ids must
# #' be passed as a data.frame and include a column \code{SeqIDs} with sequence
# #' names as well as database ids.
# #'
# #' @param mgdb MgDb class object
# #' @param db_keys (Optional) vector of database Keys of entries to include in
# #'   metagenomeAnnotation class object
# #' @param query_df (Optional) data frame with experimental data to annotate with
# #'   taxonomic information, must include column named "Key" with databse ids.
# #' @param query_seq (Optional) DNAStringSet object sequences
# #' @param mapping (Optional) method used to map sequences to database, default
# #'   "user provided", use for documenting methods used to perfom the taxonomic
# #'   assignment.
# #' @param ... additional arguments passed to select function
# #' @return metagenomeAnnotation-class object
# #' @note Must include either db_keys or query_df as argument.
# #' @rdname annotate-MgDb-method
# setGeneric("annotate", signature = "mgdb",
#            function(mgdb, ...) {
#                standardGeneric("annotate")}
# )

# #' @export
# #' @examples
# #' # see vignette
# #' @aliases annotate,MgDb-method
# #' @rdname annotate-MgDb-method
# setMethod("annotate", "MgDb",
#           function(mgdb, db_keys = NULL, query_df = NULL,
#                    query_seq = NULL, mapping = "user provided ids"){
#               .mgDb_annotate(mgdb, db_keys,
#                              query_df, query_seq, mapping)}
# )


## Annotate MRexp --------------------------------------------------------------
.mgDb_annotateMRexp_fData <- function(mgdb, MRobj){
    ## subset reference database with OTU ids
    db_keys <- featureNames(MRobj)
    db_subset <- .select(mgdb, type = "all", keys = db_keys,
                         keytype = "Keys", columns = "all")

    ## featureData
    anno_tax <- db_subset$taxa %>% as.data.frame()

    anno <- new("mgFeatures",
                data = anno_tax,
                metadata = mgdb$metadata,
                refDbSeq=db_subset$seq,
                refDbTree = db_subset$tree)
    rownames(anno@data) <- anno@data$Keys

    ## set featureData
    featureData(MRobj) <- anno
    MRobj
}


#' Annotate MRexperiment object featureData slot using MgDb object
#'
#' This method is used to define a MRexperiment object featureData slot with
#' taxonomic information from a \link[=MgDb]{MgDb-class} object using the
#' MRexperiment object's Feature names. object.
#'
#' @param mgdb MgDb class object
#' @param MRobj MRexperiment class object
#' @param ... additional arguments passed to select function
#' @return MRexperiment-class object
#' @rdname annotateMRexp_fData-MgDb-method
setGeneric("annotateMRexp_fData", signature = "mgdb",
           function(mgdb, MRobj, ...) {
               standardGeneric("annotateMRexp_fData")}
)

#' @export
#' @aliases annotateMRexp_fData,MgDb-method
#' @rdname annotateMRexp_fData-MgDb-method
setMethod("annotateMRexp_fData", "MgDb",
          function(mgdb, MRobj) .mgDb_annotateMRexp_fData(mgdb, MRobj)
)
