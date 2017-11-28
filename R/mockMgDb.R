#' Mock MgDb-class object
#'
#' Mock \link[=MgDb]{MgDb-class} object with a subset of the GreenGenes 13.9 OTU 0.99
#' database including the ids for OTU from a mock community dataset from the
#' study Bokulich et al. 2013, the OTU ids were extracted from a biom file
#' downloaded from QIITA (https://qiita.ucsd.edu).
#'
#' Bokulich, Nicholas A., et al. "Quality-filtering vastly improves diversity
#' estimates from Illumina amplicon sequencing." Nature methods 10.1 (2013): 57-59.
#'
#' @return MgDb-class object
#' @export
#' @examples
#' get_mockMgDb()
get_mockMgDb <- function(){
    ## note same as demoMgDb
    get_demoMgDb()
}
