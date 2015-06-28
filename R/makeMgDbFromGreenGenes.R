### =========================================================================
### makeMgDbFromGreenGenes()
###


.make_tmp_dir <- function(){
    tmp_dir <- paste0("mgdb_tmp",paste(sample(0:9,10, replace = T), collapse = ""))
    while(dir.exists(tmp_dir)){
        tmp_dir <- paste0("mgdb_tmp",paste(sample(0:9,10, replace = T), collapse = ""))
    }
    dir.create(tmp_dir)
    tmp_dir
}

.fetch_db <- function(db_url, tmp_dir){
    f_basename <- strsplit(db_url, split = "/") %>% unlist() %>% .[length(.)]
    f <- paste0(tmp_dir,"/",f_basename)
    download.file(db_url,destfile = f, method = "wget")
    return(f)
}

.load_taxa <- function(taxonomy_file, db_con){
    # Create the database
    taxa=read.delim(taxonomy_file,stringsAsFactors=FALSE,header=FALSE)
    keys = taxa[,1]
    taxa = strsplit(taxa[,2],split="; ")
    taxa = t(sapply(taxa,function(i){i}))
    taxa = cbind(keys,taxa)
    colnames(taxa) = c("Keys","Kingdom","Phylum","Class","Order","Family","Genus","Species")
    taxa = data.frame(taxa)
    dplyr::copy_to(db_con,taxa,temporary=FALSE, indexes=list(colnames(taxa)))
}

#' makeMgDb for GreenGenes database
#'
#' @param db_name
#' @param taxa_url
#' @param seq_url
#' @param metadata
#'
#' @return mgDb object
#' @export
#'
#' @examples makeMgDbFromGreenGenes()
makeMgDbFromGreenGenes <- function( db_name = "gg_13_5",
                                    taxa_url = "ftp://greengenes.microbio.me/greengenes_release/gg_13_5/gg_13_5_taxonomy.txt.gz",
                                    seq_url = "ftp://greengenes.microbio.me/greengenes_release/gg_13_5/gg_13_5.fasta.gz",
                                    metadata = list(URL = "https://greengenes.microbio.me",
                                                     DB_TYPE_NAME = "GreenGenes")){

                            ## defining metadata
                            metadata$ACCESSION_DATE <- Sys.Date()
                            metadata$DB_VERSION <- db_name

                            ## Retrieving DB
                            tmp_dir <- .make_tmp_dir()

                            ## getting db_seq
                            seq_file <- .fetch_db(seq_url, tmp_dir)
                            db_seq <- readDNAStringSet(seq_file)

                            ## getting db_taxa
                            db_taxa_file <- paste0(tmp_dir,"/",db_name, ".sqlite3")
                            db_con <- dplyr::src_sqlite(db_taxa_file, create = T)
                            taxonomy_file <- .fetch_db(taxa_url, tmp_dir)
                            .load_taxa(taxonomy_file, db_con)

                            ## add code/ function to move sqlite and seq data into package
                            ## pull out code to get seq and taxa db

                            ## create mgDB object
                            new("MgDb",seq = db_seq,
                                    taxa = db_taxa_file,
                                    metadata = metadata)
}

