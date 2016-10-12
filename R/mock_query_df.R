#' Example Query Data Frame
#'
#' Example \code{query_df} for use in generating a \link{mgFeatures-class}
#' object using the \link{annotateFeatures} \link[=MgDb]{MgDb-class} method.
#' The dataset contains the OTU ids and Greengenes database version 13.5 ids.
#' Specifically, the OTU centers came from a mock community dataset from the
#' study Bokulich et al. 2013, the OTU ids were extracted from a biom file
#' downloaded from QIITA (https://qiita.ucsd.edu).
#'
#' Bokulich, Nicholas A., et al. "Quality-filtering vastly improves diversity
#' estimates from Illumina amplicon sequencing." Nature methods 10.1 (2013): 57-59.
#'
#' @format data.frame
#'
#' @source
#' \url{http://bioconductor.org/packages/release/data/experiment/html/msd16s.html}
#'
#' @examples
#' data(mock_query_df)
"mock_query_df"
