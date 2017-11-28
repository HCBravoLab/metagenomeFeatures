################################################################################
##
## Definition and methods for MgDb class
## using RefClass as db changes state with query string
##
################################################################################

## MgDb Class ------------------------------------------------------------------

## Tree can be either rds with phylo class object or tree file
.load_tree <- function(tree_file){
    if (grepl("rds",tree_file)) {
        tree <- readRDS(tree_file)
    }else{
        tree <- ape::read.tree(tree_file)
    }
    tree
}

setOldClass(c("tbl_dbi"))
#' Metagenome Database class
#'
#' The MgDb-class object contains sequence, taxonomic data, and a phylogenetic
#' tree (optional) for a 16S rRNA taxonomic database, see the
#' \pkg{greengenes13.5MgDb} package as an example database. The
#' \code{get_demoMgDb} function in \pkg{metagenomeFeatures} exports a small
#' subset of the database in the \pkg{greengenes13.5MgDb} annotation package as
#' an example of a MgDb-class object.
#' @aliases mgdb
#' @slot seq database reference sequences
#' @slot tree reference phylogenetic tree
#' @slot taxa database taxonomy
#' @slot metadata associated metadata for the database
#' @export
#' @examples
#' # example MgDb-class object, a small subset of the Greengenes 13.5 database.
#' demoMgDb <- get_demoMgDb()
#' @note Currently the only database with a MgDb package is the
#'   \href{http://greengenes.secondgenome.com/}{Greengenes database} (version
#'   13.5), additional packages are planned.
#' @rdname MgDb-class
#' @return MgDb-class object
#' @importFrom dbplyr tbl_sql
#' @importClassesFrom RSQLite SQLiteConnection
setClass("MgDb",
         slots = list(seq = "SQLiteConnection",
                      taxa = "tbl_dbi",
                      tree = "phyloOrNULL",
                      ## Add db file path to metadata list
                      ## use hash to check for changes
                      metadata = "list")

)

### Create MgDb SQLite database ------------------------------------------------

#' make_mgdb
#'
#' @param db_name reference database name
#' @param db_file file path for sqlite database
#' @param taxa_tbl data frame with database taxonomy data
#' @param seqs database sequences, path to fasta file or DNAStringSet object
#'
#' @return
#' @export
#'
#' @examples
make_mgdb <- function(db_name, db_file, taxa_tbl, seqs){
    ## check parameters
    ## db_name == character string
    ## db_file == character string  - check if existing, warning if present???
    ## taxa_tbl == data frame
    ## Check taxa tbl has a valid structure - specifically taxa level order
    ## seqs either a fasta file or DNAStringSet
    ## Taxa tbl ids and string ids match and are in the same order

    ## Create database with taxa and sequence data
    db_conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)

    DECIPHER::Seqs2DB(seqs = seqs, type = "DNAStringSet",
                      dbFile = db_conn, identifier = "MgDb")

    ### Adding taxonomic data to database
    DECIPHER::Add2DB(myData = taxa_tbl, dbFile = db_conn)
}


### Initialize Function --------------------------------------------------------
#' MgDb
#'
#' @param db_file
#' @param tree
#' @param metadata
#'
#' @return MbDb class object
#' @export
#'
#' @examples
newMgDb <- function(db_file, tree, metadata){
    ## db_file is character string, file exists, is a properly formatted sqlite database
    ## Check tree is either a phylo class object or newick tree file
    ## Check metadata is list with required entries

    ## sequence slot
    db_conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)

    ## taxa slot
    taxa_dbi <- dplyr::tbl(src = db_conn, from = "Seqs")

    ## tree slot
    if (is.character(tree)) {
        tree = .load_tree(tree)
    }

    ## Return new MgDb class object
    new("MgDb",
        seq = db_conn,
        taxa = taxa_dbi,
        tree = tree,
        metadata = metadata)
}

### Validity -------------------------------------------------------------------

setValidity("MgDb", function(object) {
    msg <- NULL
    if (!("seq" %in% ls(object)) || !is(object@seq, "SQLiteConnection")) {
        msg <- paste(msg, "'seq' slot must contain DB connection", sep = "\n")
    }
    ## Add check for valid DECIPHER database structure - two table system

    if (!("taxa" %in% ls(object)) || !is(object@taxa, "tbl_dbi")) {
        msg <- paste(msg, "'taxa' slot must contain a tbl object", sep = "\n")
    }

    if (!("tree" %in% ls(object)) || (is(object@tree, "phylo") && is(object$tree, "NULL"))) {
        msg <- paste(msg, "'tree' slot must contain a phyloOrNULL object", sep = "\n")
    }

    if (!("metadata" %in% ls(object)) || !is(object@metadata, "list")) {
        msg <- paste(msg, "'metadata' slot must contain a list", sep = "\n")
    }
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
#' @return MgDb-class summary
#' @export
setMethod("show", "MgDb",
          function(object){
              cat(class(object), "object:")
              print("Metadata")
              metadata <- mgdb_meta(object)
              for (i in names(metadata)) {
                  cat("|", i, ": ", metadata[[i]], "\n", sep = "")
              }
              print("Sequence Data:")
              print("DECIPHER formatted seqDB")
              print("Taxonomy Data:")
              print(mgdb_taxa(object))
              print("Tree Data:")
              print(mgdb_tree(object))
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
#' @examples
#' demoMgDb <- get_demoMgDb()
#' mgdb_tree(demoMgDb)
mgdb_tree <- function(mgdb){
    ## Add assertion for MgDb class object
    mgdb@tree
}

#' MgDb seq slot accessor
#'
#' @param mgdb  MgDb class object
#'
#' @return DNAStringSet class object
#' @export
#'
#' @examples
#' demoMgDb <- get_demoMgDb()
#' mgdb_seq(demoMgDb)
mgdb_seq <- function(mgdb){
    ## Add assertion for MgDb class object
    mgdb@seq
}

#' MgDb taxa slot accessor
#'
#' @param mgdb  MgDb class object
#'
#' @return tbl_sql connection to sqlite table
#' @export
#'
#' @examples
#' demoMgDb <- get_demoMgDb()
#' mgdb_taxa(demoMgDb)
mgdb_taxa <- function(mgdb){
    ## Add assertion for MgDb class object
    mgdb@taxa
}

#' MgDb metadata slot accessor
#'
#' @param mgdb  MgDb class object
#'
#' @return list
#' @export
#'
#' @examples
#' demoMgDb <- get_demoMgDb()
#' mgdb_meta(demoMgDb)
mgdb_meta <- function(mgdb){
    ## Add assertion for MgDb class object
    mgdb@metadata
}
