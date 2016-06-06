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
