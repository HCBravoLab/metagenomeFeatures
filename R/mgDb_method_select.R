################################################################################
##
##                              mgDb_select method
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
    # drip.tip return class phy defining class to match mgFeature class description
    ape::drop.tip(tree,drop_tips) %>% ape::as.phylo()
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
#' mgDb_select(demoMgDb, type = "taxa",
#'      keys = c("Vibrionaceae", "Enterobacteriaceae"),
#'      keytype = "Family")
#'
#' # select seq only
#'  mgDb_select(demoMgDb, type = "seq",
#'       keys = c("Vibrionaceae", "Enterobacteriaceae"),
#'       keytype = "Family")
#'
#' # select all taxa, seq, and tree
#' mgDb_select(demoMgDb, type = "all",
#'        keys = c("Vibrionaceae", "Enterobacteriaceae"),
#'        keytype = "Family")
#' @rdname select-MgDb-method
setGeneric("mgDb_select", signature="mgdb",
           function(mgdb, type, ...) { standardGeneric("mgDb_select")
           })


#' @export
#' @aliases select,MgDb-method
#' @rdname select-MgDb-method
setMethod("mgDb_select", "MgDb",
          function(mgdb, type, keys = NULL, keytype = NULL, columns = "all"){
              .select(mgdb, type, keys, keytype, columns)
          }
)
