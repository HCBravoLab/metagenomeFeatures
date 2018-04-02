################################################################################
##
## Definition and methods for MgDb class
## using RefClass as db changes state with query string
##
################################################################################

## MgDb Class ------------------------------------------------------------------

## Tree can be either rds with phylo class object or tree file
.load_tree <- function(tree_file){
    if (!file.exists(tree_file)) {
        stop(paste0("tree_file:", tree_file, " is not a valid file path"))
    }

    if (grepl("rds",tree_file,ignore.case = TRUE)) {
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
#' \code{get_gg13.8_85MgDb()} function in \pkg{metagenomeFeatures} exports a
#' small subset of the database in the \pkg{greengenes13.5MgDb} annotation
#' package as an example MgDb-class object.
#' @aliases mgdb
#' @slot seq database reference sequences
#' @slot tree reference phylogenetic tree
#' @slot taxa database taxonomy
#' @slot metadata associated metadata for the database
#' @export
#' @examples
#' # example MgDb-class object, Greengenes 13.8 85% OTUs database.
#' gg85 <- get_gg13.8_85MgDb()
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

#' make_mgdb_sqlite
#'
#' @param db_name reference database name
#' @param db_file file path for sqlite database
#' @param taxa_tbl data frame with database taxonomy data
#' @param seqs database sequences, path to fasta file or DNAStringSet object
#'
#' @return writes SQLite file
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' make_mgdb_sqlite(db_name = "greengenes13.8_85",
#'     db_file = db_file,
#'     taxa_tbl = taxa_tbl,
#'     seqs = seqs)
#'}

make_mgdb_sqlite <- function(db_name, db_file, taxa_tbl, seqs) {
    ## Parameter check -----------------------------------------
    ## db_name - character string
    if (!(is.character(db_name) & length(db_name) == 1)) {
        stop("db_name must be a character string")
    }

    ## db_file == character string  - check if existing, warning if present
    if (!(is.character(db_file) & length(db_file) == 1)) {
        stop("db_name must be a character string")
    }

    if (file.exists(db_file)) {
        warning("db_file exists adding sequence and taxa data will be added to the database")
    }

    ## taxa_tbl == data frame
    if (!is.data.frame(taxa_tbl)) {
        stop("taxa_tbl must be a data frame")
    }

    ## Check taxa tbl has a valid structure - specifically taxa level order
    ## seqs either a fasta file or DNAStringSet
    if (is.character(seqs)) {
        if (file.exists(seqs)) {
            seqs <- Biostrings::readDNAStringSet(seqs)
        } else {
            stop("seqs is a character string but no file exists, check filename")
        }
    } else if (!is(seqs, "DNAStringSet")) {
           stop("seqs must be either a DNAStringSet class object or path to a fasta file")
    }

    ### Check taxa and string keys match
    taxa_keys <- taxa_tbl$Keys
    seq_keys <- names(seqs)
    if (length(taxa_keys) != length(seq_keys)) {
        stop("taxa_tbl$Keys and names(seqs) must match")
    }

    if (sum(taxa_keys %in% seq_keys) != length(taxa_keys)) {
        stop("taxa_tbl$Keys and names(seqs) must match")
    }

    if (sum(seq_keys %in% taxa_keys) != length(seq_keys)) {
        stop("taxa_tbl$Keys and names(seqs) must match")
    }

    if (length(unique(taxa_keys)) != length(taxa_keys)) {
        stop("taxa_tbl$Keys must be unique")
    }

    ## Taxa tbl ids and string ids match and are in the same order
    taxa_tbl$Keys <- as.character(taxa_tbl$Keys)
    taxa_tbl <- taxa_tbl[match(names(seqs), taxa_tbl$Keys),]
    rownames(taxa_tbl) <- 1:nrow(taxa_tbl)

    ## Create database with taxa and sequence data
    db_conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)

    DECIPHER::Seqs2DB(seqs = seqs, type = "DNAStringSet",
                      dbFile = db_conn, identifier = "MgDb")

    ### Adding taxonomic data to database

    # get seqs table from database
    db_seqs <- RSQLite::dbReadTable(db_conn, "Seqs")

    taxa_tbl$row_names <- seq(1:nrow(taxa_tbl))

    # merge taxa data with seq table
    db_merge_table <- merge(db_seqs, taxa_tbl, by='row_names')

    # write seq data back to database
    RSQLite::dbWriteTable(db_conn, "Seqs", db_merge_table, overwrite=TRUE)

    # DECIPHER::Add2DB(myData = taxa_tbl, dbFile = db_conn)

    RSQLite::dbDisconnect(db_conn)
}


### Initialize Function --------------------------------------------------------
#' MgDb
#'
#' @param db_file SQLite filename with database taxonomy and sequence data
#' @param tree newick filename with database tree data
#' @param metadata list with database metadata
#'
#' @return MbDb class object
#' @export
#'
#' @examples
#' metadata_file <- system.file("extdata", 'gg13.8_85_metadata.RData',
#'     package = "metagenomeFeatures")
#' load(metadata_file)
#'
#' gg_db_file <- system.file("extdata", 'gg13.8_85.sqlite',
#'                           package = "metagenomeFeatures")
#'
#' gg_tree_file <- system.file("extdata", "gg13.8_85.tre",
#'                             package = "metagenomeFeatures")
#'
#' ## Creating a new MgDb class object with gg13.8_85 data
#' newMgDb(db_file = gg_db_file,
#'         tree = gg_tree_file,
#'         metadata =  metadata)
#'
newMgDb <- function(db_file, tree, metadata){
    ## db_file is character string, file exists, is a properly formatted sqlite
    ## database Check tree is either a phylo class object or newick tree file
    ## Check metadata is list with required entries

    ## sequence slot
    db_conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_file)

    ## taxa slot
    taxa_dbi <- dplyr::tbl(src = db_conn, from = "Seqs")

    ## tree slot
    if (!(is.character(tree) | is.null(tree))) {
        stop("Tree must be NULL or character string with tree file path")
    }

    if (is.character(tree)) {
        ## For consistency with MgDb version 1 tree slot definition
        if (tree == "not available") {
            tree <- NULL
        } else {
            tree <- .load_tree(tree)
        }
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
    if (!("seq" %in% slotNames(object)) || !is(object@seq, "SQLiteConnection")) {
        msg <- paste(msg, "'seq' slot must contain DB connection", sep = "\n")
    }
    ## Add check for valid DECIPHER database structure - two table system

    if (!("taxa" %in% slotNames(object)) || !is(object@taxa, "tbl_dbi")) {
        msg <- paste(msg, "'taxa' slot must contain a tbl object", sep = "\n")
    }
    ## Add checks for taxa heirarchy.

    if (!("tree" %in% slotNames(object)) || (is(object@tree, "phylo") && is(object@tree, "NULL"))) {
        msg <- paste(msg, "'tree' slot must contain a phyloOrNULL object", sep = "\n")
    }
    ## Add checks for tree structure

    if (!("metadata" %in% slotNames(object)) || !is(object@metadata, "list")) {
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
              metadata <- mgDb_meta(object)
              for (i in names(metadata)) {
                  cat("|", i, ": ", metadata[[i]], "\n", sep = "")
              }
              print("Sequence Data:")
              print("DECIPHER formatted seqDB")
              print("Taxonomy Data:")
              print(mgDb_taxa(object))
              print("Tree Data:")
              if (!is.null(mgDb_tree(object))) {
                  ape::print.phylo(mgDb_tree(object))
              } else {
                  print("Tree not available")
              }
          }
)

## Accessors -------------------------------------------------------------------

#' MgDb-class accessors
#'
#' Accessors for \linkS4class{MgDb}-class object slots. \code{mgDb_seq} -
#' sequence slot, \code{mgDb_taxa} - taxa slot, \code{mgDb_tree} - phylogenetic
#' tree slot, and \code{mgDb_meta} - metadata slot.
#'
#' @name mgDb_
#' @param mgdb MgDb-class object.
#'
#' @return appropriate class object for the slot accessed
#' @examples
#' gg85 <- get_gg13.8_85MgDb()
#' mgDb_seq(gg85)
#' mgDb_taxa(gg85)
#' mgDb_tree(gg85)
#' mgDb_meta(gg85)
NULL


# MgDb tree slot accessor
#' @export
#' @rdname mgDb_
mgDb_tree <- function(mgdb){
    ## Add assertion for MgDb class object
    mgdb@tree
}

# MgDb seq slot accessor
#' @export
#' @rdname mgDb_
mgDb_seq <- function(mgdb){
    ## Add assertion for MgDb class object
    mgdb@seq
}

# MgDb taxa slot accessor
#' @export
#' @rdname mgDb_
mgDb_taxa <- function(mgdb){
    ## Add assertion for MgDb class object
    mgdb@taxa
}

# MgDb metadata slot accessor
#' @export
#' @rdname mgDb_
mgDb_meta <- function(mgdb){
    ## Add assertion for MgDb class object
    mgdb@metadata
}
