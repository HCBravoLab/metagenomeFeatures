#' Aggregates a MRexperiment object and returns either an aggregated
#' MRexperiment or counts matrix to a user defined taxonomic level.
#'
#' Using the featureData information in the MRexperiment-class object,
#' aggregate_taxa aggregates the OTU count data (MRexperiment assayData slot)
#' to a user defined taxonomic level (i.e. 'genus') using the defined
#' aggfun function (default colSums). Possible aggfun alternatives include and
#' column wise matrix calculations, e.g. colMeans, colMedians.
#'
#' @param obj A MRexperiment-class object or count matrix.
#' @param lvl featureData column name from the MRexperiment object or if count
#'   matrix object a vector of labels.
#' @param aggfun Matrix aggregation function, e.g. colSums.
#' @param out Either 'MRexperiment' or 'matrix'
#' @param ... Additional parameters to pass to MRcount, e.g. norm, log, and sl.
#' @return An aggregated count matrix or MRexperiment
#' @rdname aggregate_taxa
#' @importClassesFrom metagenomeSeq MRexperiment
#' @export
#' @examples
#' data("mouseData", package = "metagenomeSeq")
#' aggregate_taxa(mouseData[1:100,],lvl="class",norm=TRUE,aggfun=colSums)
#' aggregate_taxa(mouseData,lvl="class",norm=TRUE,aggfun=colSums)
#' aggregate_taxa(mouseData,lvl='phylum',norm=FALSE,aggfun=colSums)
aggregate_taxa <-function(obj, lvl, aggfun = colSums, out="MRexperiment", ...){
    ##### check parameters
    if(class(obj)!="MRexperiment"){
        stop("Input must either be of class 'MRexperiment'")
    }

    if(dims(obj)[2] == 1){
        stop("aggregateByTaxonomy does not currently work for single sample MRexperiment objects.")
    }

    if(!is.character(lvl) && !(lvl %in% taxa_levels(obj))){
        stop(paste(lvl, "not a taxonomic level in MRexp featureData, use `taxa_levels` to for a list a appropriate levels for use in aggregating"))
    }
    ## Not defined as function argument - checks should move to MRcounts
    # if(!is.logical(norm)){
    #     stop(past0("norm ", bool_msg))
    # }

    # if(exists(log)    !is.logical(log)){
    #     stop(past0("log ", bool_msg))
    # }

    # if(!(is.integer(sl))|| sl < 1){
    #     ## does this check even make sense?
    #     ## why does sl throw error if out parameter defined?
    #     warning("The parameter `sl` must either be a positive integer, see `MRcounts for more information")
    # }

    if(!(out%in%c("MRexperiment","matrix"))){
        stop("The parameter `out` must either be 'MRexperiment' or 'matrix'")
    }

    ## count values to aggregate
    count_mat <- metagenomeSeq::MRcounts(obj,...)

    ## aggregator vector
    # - using all taxonomic levels to lowest defined level
    # - assumes taxonomic heirarchy is from left to right
    # - with highest level (e.g. Domain) is the left most column
    # - and lowest level (e.g. species) in the right most column
    # - except for OTU and Key ID columns which maybe first (as in MgDb class)

    ## extract featureData as data.frame
    obj_df <- obj %>% fData()
    ## for when NAs are character strings/ factors
    obj_df[obj_df == "NA"] <- NA

    # checking for  and removing OTU/ Key level column(s)
    # - columns where number with unique values for every row
    index_colnames <- obj_df %>% as.list() %>%
        purrr::map_int(dplyr::n_distinct) %>%
        .[. == nrow(obj_df)] %>% names()
    ## - what to do, if anything, when more than one otu_col
    ## - Might want to consider ordering columns based on
    ##   increasing number of unique values, or standard taxonomy

    if(length(index_colnames) == ncol(obj_df)){
        message("Taxa values for all levels are unique, no data to aggregate")
        if(out == "matrix") return(count_mat)
        return(obj)
    }

    ## user define lvl values all unique
    if(lvl %in% index_colnames){
        message("Taxa values for specified level are all unique, no data to aggregate")
        if(out == "matrix") return(count_mat)
        return(obj)
    }

    ## removing index columns so they don't interfere with aggregation
    obj_df[[index_colnames]] <- NULL

    ## finds the highest index position for value in lvl for subsetting
    obj_levels <- obj_df %>% colnames()
    lvl_index <- which.max(obj_levels == lvl)

    ## creating index vector for aggregating
    levels <- obj_df[,1:lvl_index] %>% unique()
    levels$index <- 1:nrow(levels)
    agg_vector <- merge(obj_df, levels)$index

    ## perform aggregation
    agg_vals <- c()
    for(i in 1:max(agg_vector)){
        sub_mat <- count_mat[agg_vector == i,]
        if(nrow(sub_mat) %>% is.null()){
            agg_sub <- sub_mat
        }else{
            agg_sub <- aggfun(sub_mat)
        }
        agg_vals <- c(agg_vals,agg_sub)
    }
    count_agg <- matrix(agg_vals, ncol = dim(count_mat)[2], byrow = TRUE)
    colnames(count_agg) <- colnames(count_mat)

    ## Generate new unique row names
    agg_lvl_names <- levels[,lvl_index] %>% as.character()
    for(i in (lvl_index-1):1){
        na_index <- is.na(agg_lvl_names)
        if(sum(na_index) > 0){
            replace_index <- !is.na(levels[,i]) == na_index
            agg_lvl_names[replace_index] <- paste0("unk_",levels[replace_index,i])
        }else{
            break
        }
    } # need to add check for non-unique values, all values should be unique
    rownames(count_agg) <- agg_lvl_names


    if(out=='matrix') return(count_agg)
    if(out=='MRexperiment'){
        taxa <- levels %>% dplyr::select(-index) %>% as("AnnotatedDataFrame")
        rownames(taxa) <- agg_lvl_names

        if(class(obj)=="MRexperiment"){
            pd = phenoData(obj)
            newObj = newMRexperiment(count_agg,featureData=taxa,phenoData=pd)
        } else {
            newObj = newMRexperiment(count_agg,featureData=taxa)
        }
        return(newObj)
    }
}


## helper functions
#' Accessor function for taxonomic levels in MRexperiment featureData
#'
#' @param obj an `MRexperiment-class` object
#'
#' @return character vector with taxonomic levels
#' @export
#' @import metagenomeSeq
#' @examples
#' data("mouseData", package = "metagenomeSeq")
#' taxa_levels(mouseData)
taxa_levels <- function(obj){
    if(class(obj)!="MRexperiment") stop("Input must either be of class 'MRexperiment'")
    obj %>% fData() %>% colnames()
}
