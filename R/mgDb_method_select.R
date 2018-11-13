################################################################################
##
##                              mgDb_select method
##
################################################################################
## Select ----------------------------------------------------------------------

# #' select sequences
# #' @param seqObj seq object
# #' @param row_names row names to select
# #' @param Keys keys to filter
#' @importClassesFrom Biostrings DNAStringSet
#' @importFrom Biostrings DNAStringSet
#' @importFrom RSQLite dbGetQuery
#' @importFrom DECIPHER Codec
# #' @keywords internals
.select.seq <- function(seqObj, row_names, Keys){
    result <- RSQLite::dbGetQuery(seqObj, paste("select row_names, sequence from _Seqs where row_names in ", paste0("(", paste0(row_names, collapse = ','), ")")))
    seqs <- DECIPHER::Codec(result$sequence)
    dnaStringSet <- Biostrings::DNAStringSet(seqs)
    names(dnaStringSet) <- Keys[match(result[,1], row_names)]

    dnaStringSet
}

# #' select taxa
# #' @param taxaDb taxa object
# #' @param metaDb metadata object
# #' @param keytype keytypes to select
# #' @param keys keys to filter
# #' @param columns columns to select, defaults to all
#' @importFrom dplyr select
#' @importFrom dplyr select_
#' @importFrom dplyr filter
#' @importFrom dplyr collect
#' @importFrom stringr str_c
#' @importFrom stringr str_sub
#' @importFrom lazyeval interp
# #' @keywords internals
.select.taxa <- function(taxaDb, metaDb, keys, keytype, columns="all"){
    ## setting values when keys and keytypes are not defined
    if (is.null(keys)) {
        key_df <-  dplyr::select(taxaDb, Keys) %>% dplyr::collect()
        keys <- key_df$Keys
        keytype <- "Keys"
    }

    # selecting desired rows
    if (!is.null(keys)) {
        if (keytype !=  "Keys") {
            level_id <- stringr::str_sub(string = keytype,
                                         start = 1,
                                         end = 1) %>%
                tolower() %>% rep(length(keys))
            if (metaDb$DB_TYPE_NAME == "GreenGenes") {
                keys <- stringr::str_c(level_id,keys,sep = "__")
            }
        }

        if (length(keys) == 1) {
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
    if (columns[1] != "all") {
        select_tbl <- dplyr::select_(select_tbl, .dots = columns)
    }

    return(dplyr::collect(select_tbl))
}

# #' select tree tips
# #' @param tree tree object
# #' @param ids tip ids to select
#' @importFrom ape drop.tip
#' @importFrom ape as.phylo
# #' @keywords internals
.select.tree <- function(tree, ids){
    drop_tips <- tree$tip.label[!(tree$tip.label %in% ids)]
    # drop.tip return class phy defining
    # class to match mgFeature class description
    ape::drop.tip(tree,drop_tips) %>% ape::as.phylo()
}

# #' either select by ids for taxa information
# #' @param mgdb mgdb object
# #' @param type selection type, can be seq, taxa, tree or all
# #' @param keytype keytypes to select
# #' @param keys keys to filter
# #' @param columns columns to select, defaults to all
#' @importFrom dplyr select_
# #' @keywords internals
.select <- function(mgdb, type, keys, keytype, columns){
    ## check correct types
    select_types <- c("seq","taxa", "tree", "all")
    if (FALSE %in% (type %in% select_types)) {
        bad_type <- type[!(type %in% select_types)]
        stop(paste(bad_type, "not valid type value, type must be either 'seq',",
                    "'taxa', 'tree', 'all' or a character vector with types"))
    }

    if (is.null(keys) != is.null(keytype)) {
        stop("must define both keys and keytypes, or neither")
    }

    ## list with select results
    select_obj <- list()
    taxa_df <- .select.taxa(mgDb_taxa(mgdb), mgDb_meta(mgdb),
                            keys, keytype, columns)

    ## Warning if no query matches
    if (nrow(taxa_df) == 0) {
        warning(paste("mgDb_select did not match any",
                      paste(keys, collapse = ", "), "from", keytype))
    }

    ## Extracting ids for subsetting seqs
    row_names <- taxa_df$row_names

    # vector with all objects to return
    if ("taxa" %in% type || type == "all") {
        ## Removing decipher columns
        ## avoiding namespace conflict with colname arguments in select
        description <- NULL
        identifier <- NULL
        select_obj$taxa <- dplyr::select(taxa_df, -row_names,
                                         -description, -identifier)
    }

    if ("seq" %in% type || type == "all") {
        select_obj$seq <- .select.seq(mgDb_seq(mgdb),
                                      taxa_df$row_names, taxa_df$Keys)
    }

    if ("tree" %in% type || type == "all") {
        select_obj$tree <- .select.tree(mgDb_tree(mgdb), taxa_df$Keys)
    }

    ## return single obj if only selecting one type
    if (type != "all" && length(type)  == 1) {
        select_obj <- select_obj[[type]]
    }

    return(select_obj)
}

#' Querying MgDb objects
#'
#' Function for querying \code{\link{MgDb-class}} class objects, user defines
#' the taxonomic levels (\code{keytype}) and a vector of taxonomic names
#' (\code{keys}) being selected. If specific database ids are being selected for
#' use \code{keytype="Keys"}. Additionally, users can specify whether they want
#' only the taxonomic and sequence data, or both.
#'
#' @param mgdb MgDb class object
#' @param type either "taxa", "seq", "tree", "all" or a character vector of
#'   types. "taxa", "seq", and "tree" only query the reference taxonomy,
#'   sequences, and phylogenetic tree respectively. "all" queries the reference
#'   taxonomy, sequence, and phylogenetic tree.
#' @param keys specific taxonomic groups to select for
#' @param keytype taxonomic level of keys
#' @param columns keytypes in taxonomy databse to return, all by default
#' @param ... additional arguments passed to select function
#' @return returned object depends on type: 'taxa' - dataframe with taxa
#'   information; 'seq' - DNAStringSet with seqeunce data; 'tree' - phylogenetic
#'   tree of class phylo; 'all' - list with the dataframe, DNAStringSet, and
#'   phylo.
#' @examples
#' gg85 <- get_gg13.8_85MgDb()
#' # select taxa only
#' mgDb_select(gg85, type = "taxa",
#'      keys = c("Vibrionaceae", "Enterobacteriaceae"),
#'      keytype = "Family")
#'
#' # select seq only
#'  mgDb_select(gg85, type = "seq",
#'       keys = c("Vibrionaceae", "Enterobacteriaceae"),
#'       keytype = "Family")
#'
#' # select all taxa, seq, and tree
#' mgDb_select(gg85, type = "all",
#'        keys = c("Vibrionaceae", "Enterobacteriaceae"),
#'        keytype = "Family")
#' @rdname select-MgDb-method
setGeneric("mgDb_select", signature = "mgdb",
           function(mgdb, type, ...) {standardGeneric("mgDb_select")}
)


#' @export
#' @aliases select,MgDb-method
#' @rdname select-MgDb-method
setMethod("mgDb_select", "MgDb",
          function(mgdb, type, keys = NULL, keytype = NULL, columns = "all"){
              .select(mgdb, type, keys, keytype, columns)
          }
)
