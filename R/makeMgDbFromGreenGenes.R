### =========================================================================
### makeMgDbFromGreenGenes()
###
### modeled after makeTranscriptDbFromUCSC
### -------------------------------------------------------------------------

### The 2 main tasks that makeTranscriptDbFromUCSC() performs are:
###   (1) download the data from UCSC into a data.frame (the getTable() call);
###   (2) store that data.frame in an SQLite db (the
###       .makeTranscriptDbFromUCSCTxTable() call).
### Speed:
###   - for genome="hg18" and tablename="knownGene":
###       (1) download takes about 40-50 sec.
###       (2) db creation takes about 30-35 sec.
makeTranscriptDbFromUCSC <- function(genome="hg19",
                                     tablename="knownGene",
                                     transcript_ids=NULL,
                                     circ_seqs=DEFAULT_CIRC_SEQS,
                                     url="http://genome.ucsc.edu/cgi-bin/",
                                     goldenPath_url="http://hgdownload.cse.ucsc.edu/goldenPath",
                                     miRBaseBuild=NA)
{
    track <- .tablename2track(tablename, genome)
    if (!is.null(transcript_ids)) {
        if (!is.character(transcript_ids) || any(is.na(transcript_ids)))
            stop("'transcript_ids' must be a character vector with no NAs")
    }
    if (!isSingleString(url))
        stop("'url' must be a single string")
    if (!isSingleString(goldenPath_url))
        stop("'goldenPath_url' must be a single string")

    ## Create an UCSC Genome Browser session.
    session <- browserSession(url=url)
    genome(session) <- genome
    track_tables <- tableNames(ucscTableQuery(session, track=track))
    if (!(tablename %in% track_tables))
        stop("GenomicFeatures internal error: ", tablename, " table doesn't ",
             "exist or is not associated with ", track, " track. ",
             "Thank you for reporting this to the GenomicFeatures maintainer ",
             "or to the Bioconductor mailing list, and sorry for the ",
             "inconvenience.")

    ## Download the transcript table.
    message("Download the ", tablename, " table ... ", appendLF=FALSE)
    query <- ucscTableQuery(session, track, table=tablename,
                            names=transcript_ids)
    ucsc_txtable <- getTable(query)
    if (ncol(ucsc_txtable) < length(.UCSC_TXCOL2CLASS))
        stop("GenomicFeatures internal error: ", tablename, " table doesn't ",
             "exist, was corrupted during download, or doesn't contain ",
             "transcript information. ",
             "Thank you for reporting this to the GenomicFeatures maintainer ",
             "or to the Bioconductor mailing list, and sorry for the ",
             "inconvenience.")
    message("OK")

    ## Get the tx_name-to-gene_id mapping.
    mapdef <- .howToGetTxName2GeneIdMapping(tablename)
    if (is.null(mapdef)) {
        txname2geneid <- list(genes=NULL, gene_id_type="no gene ids")
    } else if (is.list(mapdef)) {
        txname2geneid <- .fetchTxName2GeneIdMappingFromUCSC(session, track,
                                                            tablename, mapdef)
    } else if (is.character(mapdef)) {
        txname2geneid <- .extractTxName2GeneIdMappingFromUCSCTxTable(
            ucsc_txtable, mapdef)
    } else {
        stop("GenomicFeatures internal error: invalid 'mapdef'")
    }

    .makeTranscriptDbFromUCSCTxTable(ucsc_txtable, txname2geneid$genes,
                                     genome, tablename,
                                     txname2geneid$gene_id_type,
                                     full_dataset=is.null(transcript_ids),
                                     circ_seqs=circ_seqs,
                                     goldenPath_url=goldenPath_url,
                                     miRBaseBuild=miRBaseBuild)
}

## from old annotation development
## creating the sqlite database
load_tax <- function(taxonomyFile){
    # Create the database
    tree=read.delim(taxonomyFile,stringsAsFactors=FALSE,header=FALSE)
    keys = tree[,1]
    tree = strsplit(tree[,2],split="; ")
    tree = t(sapply(tree,function(i){i}))
    tree = cbind(keys,tree)
    colnames(tree) = c("Keys","Kingdom","Phylum","Class","Order","Family","Genus","Species")
    tree = data.frame(tree)

    my_db <- dplyr::src_sqlite("taxaDb.sqlite3", create = T)
    tree_sqlite<- dplyr::copy_to(my_db,tree,
                                 temporary=FALSE,
                                 indexes=list(colnames(tree)))
}

#load_tax("gg_13_5_taxonomy.txt.gz")
