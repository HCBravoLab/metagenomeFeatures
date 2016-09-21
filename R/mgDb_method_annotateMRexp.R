## Annotate MRexp --------------------------------------------------------------
.mgDb_annotateMRexp_fData <- function(mgdb, MRobj){
    ## check for tree slot
    if(is.null(mgdb$tree)){
        select_type = c("seq","taxa")
    }else{
        select_type = "all"
    }


    ## subset reference database with OTU ids
    db_keys <- featureNames(MRobj)
    db_subset <- .select(mgdb, type = select_type, keys = db_keys,
                         keytype = "Keys", columns = "all")

    ## featureData
    anno_tax <- db_subset$taxa %>% as.data.frame()
    if(select_type != "all"){
        anno <- new("mgFeatures",
                    data = anno_tax,
                    metadata = mgdb$metadata,
                    refDbSeq=db_subset$seq,
                    refDbTree=NULL)
    }else{ # for when no ref tree is provided
        anno <- new("mgFeatures",
                    data = anno_tax,
                    metadata = mgdb$metadata,
                    refDbSeq=db_subset$seq,
                    refDbTree = db_subset$tree)
    }
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
