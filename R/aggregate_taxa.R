### ============================================================================
##
##                              Taxa Aggregation methods
##
### ============================================================================

# MgDb taxa_aggregation method

.mgDb_aggregate_taxonomy <- function(mgdb, taxa_level = NULL, mapping = FALSE){
    if (class(mgdb) != "MgDb") {
        stop("mgdb parameter not correct class")
    }
    if (is.null(taxa_level)) {
        return(list(agg_taxa_table = NULL, agg_taxa_mapping = NULL))
    }
    taxa_table <- mgdb_taxa(mgdb)

    if (!(taxa_level %in% colnames(taxa_table))) {
        stop("taxa_level not part of taxa_table")
    }
    if (mapping && !is.logical(mapping)) {
        stop("mapping needs to be logical value")
    }

    aggregated_taxa <- colnames(taxa_table)[1:which(colnames(taxa_table) == taxa_level)]
    ret_table <- dplyr::select(taxa_table, tidyselect::one_of(aggregated_taxa))

    if (mapping) {
        mapping <- dplyr::select(taxa_table, tidyselect::one_of(c("Keys", taxa_level)))
        return(list(agg_taxa_table = ret_table, agg_taxa_mapping = mapping))
    }
    return(ret_table)
}

#' Aggregate MgDb class object to a taxonomic level
#'
#' Returns a tibble with distinct rows containing the taxonomic heirarchy to a
#' user defined taxonomic level. Optionally a mapping tibble for use in
#' aggregating counts, with two columns one with values for user defined
#' taxonomic level and sequence keys.
#'
#' @name mgDb_aggregate_taxonomy
#' @param mgdb object of MgDB class
#' @param taxa_level taxonomic aggregation level
#' @param mapping logical for returning mapping of ids to aggregated taxa level
#' @return tbl_df or list(tbl_df, tbl_df)
#'
#' @examples
#' gg97 <- get_gg13.8_97MgDb()
#' mgDb_aggregate_taxonomy(gg97, taxa_level = "Phylum")
#' @exportMethod mgDb_aggregate_taxonomy
#' @rdname mgDb_aggregate_taxonomy
setGeneric("mgDb_aggregate_taxonomy", signature = "mgdb",
           function(mgdb, ...) standardGeneric("mgDb_aggregate_taxonomy"))

## MgDb taxa_keys method
#' @rdname mgDb_aggregate_taxonomy
#' @aliases mgDb_aggregate_taxonomy,MgDb-method
setMethod("mgDb_aggregate_taxonomy", "MgDb",
          function(mgdb, ...) .mgDb_aggregate_taxonomy(mgdb, ...))


# mgFeatures taxa_aggregation method

.mgF_aggregate_taxonomy <- function(mgf, taxa_level = NULL, mapping = FALSE){
    if (class(mgf) != "mgFeatures") {
        stop("mgf parameter not correct class")
    }
    if (is.null(taxa_level)) {
        return(list(agg_taxa_table = NULL, agg_taxa_mapping = NULL))
    }
    taxa_table <- mgF_taxa(mgf)

    if (!(taxa_level %in% colnames(taxa_table))) {
        stop("taxa_level not part of taxa_table")
    }
    if (mapping && !is.logical(mapping)) {
        stop("mapping needs to be logical value")
    }

    aggregated_taxa <- colnames(taxa_table)[1:which(colnames(taxa_table) == taxa_level)]
    ret_table <- taxa_table[,aggregated_taxa]

    if (mapping) {
        mapping <- taxa_table[,c("Keys", taxa_level)]
        return(list(agg_taxa_table = ret_table, agg_taxa_mapping = mapping))
    }
    return(ret_table)
}

#' Aggregate mgFeatures class object to a taxonomic level
#'
#' Returns a tibble with distinct rows containing the taxonomic heirarchy to a
#' user defined taxonomic level. Optionally a mapping tibble for use in
#' aggregating counts, with two columns one with values for user defined
#' taxonomic level and sequence keys.
#'
#' @name mgF_aggregate_taxonomy
#' @param mgf object of mgFeatures class
#' @param taxa_level taxonomic aggregation level
#' @param mapping logical for returning mapping of ids to aggregated taxa level
#' @return list(tbl_df, tbl_df)
#'
#' @examples
#' data(mock_mgF)
#' mgF_aggregate_taxonomy(mock_mgF, taxa_level = "Phylum")
#' @exportMethod mgF_aggregate_taxonomy
#' @rdname mgF_aggregate_taxonomy
setGeneric("mgF_aggregate_taxonomy", signature = "mgf",
           function(mgf, ...) standardGeneric("mgF_aggregate_taxonomy"))

## MgF aggregate_taxa method
#' @rdname mgF_aggregate_taxonomy
#' @aliases mgF_aggregate_taxonomy
setMethod("mgF_aggregate_taxonomy", "mgFeatures",
          function(mgf, ...) .mgF_aggregate_taxonomy(mgf, ...))


