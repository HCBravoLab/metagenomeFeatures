### ============================================================================
##
##                              Taxa Aggregation methods
##
### ============================================================================

# MgDb taxa_aggregation method

.aggregate_taxonomy<-function(mgdb, taxa_level = NULL, mapping = FALSE){
    if(class(mgdb)!="MgDb"){
        stop("mgdb parameter not correct class")
    }
    if(is.null(taxa_level)){
        return(list(agg_taxa_table = NULL, agg_taxa_mapping = NULL))
    }
    taxa_table <- mgdb_taxa(mgdb)

    if(!(taxa_level %in% colnames(taxa_table))){
        stop("taxa_level not part of taxa_table")
    }
    if(mapping && !is.logical(mapping)){
        stop("mapping needs to be logical value")
    }

    aggregated_taxa <- colnames(taxa_table)[1:which(colnames(taxa_table) == taxa_level)]
    ret_table <- dplyr::select(taxa_table, tidyselect::one_of(aggregated_taxa))

    if(mapping){
        mapping <- dplyr::select(taxa_table, one_of(c("Keys", taxa_level)))
        return(list(agg_taxa_table = ret_table, agg_taxa_mapping = mapping))
    }
    return(list(agg_taxa_table = ret_table, agg_taxa_mapping = NULL))
}

#' Taxonomy aggregation for a given level
#'
#' @name aggregate_taxonomy
#' @param mgdb object of MgDB class
#' @param taxa_level taxonomic aggregation level
#' @param mapping logical for returning mapping of ids to aggregated taxa level
#' @return list(tbl_df, tbl_df)
#'
#' @examples
#' gg97 <- get_gg13.8_97MgDb()
#' aggregate_taxonomy(gg97, taxa_level = "Phylum")
#' @exportMethod aggregate_taxonomy
#' @rdname aggregate_taxonomy
setGeneric("aggregate_taxonomy", signature = "mgdb",
           function(mgdb, ...) standardGeneric("aggregate_taxonomy"))

## MgDb taxa_keys method
#' @rdname aggregate_taxonomy
#' @aliases aggregate_taxonomy,MgDb-method
setMethod("aggregate_taxonomy", "MgDb",
          function(mgdb, ...) .aggregate_taxonomy(mgdb, ...))


# mgFeatures taxa_aggregation method

.aggregate_taxonomy_mgf<-function(mgf, taxa_level = NULL, mapping = FALSE){
    if(class(mgf)!="mgFeatures"){
        stop("mgf parameter not correct class")
    }
    if(is.null(taxa_level)){
        return(list(agg_taxa_table = NULL, agg_taxa_mapping = NULL))
    }
    taxa_table <- mgF_taxa(mgf)

    if(!(taxa_level %in% colnames(taxa_table))){
        stop("taxa_level not part of taxa_table")
    }
    if(mapping && !is.logical(mapping)){
        stop("mapping needs to be logical value")
    }

    aggregated_taxa <- colnames(taxa_table)[1:which(colnames(taxa_table) == taxa_level)]
    ret_table <- taxa_table[,aggregated_taxa]

    if(mapping){
        mapping <- taxa_table[,c("Keys", taxa_level)]
        return(list(agg_taxa_table = ret_table, agg_taxa_mapping = mapping))
    }
    return(list(agg_taxa_table = ret_table, agg_taxa_mapping = NULL))
}

#' Taxonomy aggregation for a given level
#'
#' @name aggregate_taxonomy_mgf
#' @param mgf object of mgFeatures class
#' @param taxa_level taxonomic aggregation level
#' @param mapping logical for returning mapping of ids to aggregated taxa level
#' @return list(tbl_df, tbl_df)
#'
#' @examples
#' data(mock_mgF)
#' aggregate_taxonomy_mgf(mock_mgF, taxa_level = "Phylum")
#' @exportMethod aggregate_taxonomy_mgf
#' @rdname aggregate_taxonomy_mgf
setGeneric("aggregate_taxonomy_mgf", signature = "mgf",
           function(mgf, ...) standardGeneric("aggregate_taxonomy_mgf"))

## MgF aggregate_taxa method
#' @rdname aggregate_taxonomy_mgf
#' @aliases aggregate_taxonomy_mgf
setMethod("aggregate_taxonomy_mgf", "mgFeatures",
          function(mgf, ...) .aggregate_taxonomy_mgf(mgf, ...))


