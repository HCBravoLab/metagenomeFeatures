## Not currently used - feature to add
## Code for creating mg.db packages from different database sources
##
## Modeled after makeTxDbPackage.R in GenomicFeatures package
## Mostly copy and pasted at this point ....
## uses Biobase createPackage function to generate packages

## simplify DB retrievals from metadata table
# .getMetaDataValue <- function(txdb, name){
#     con <- AnnotationDbi:::dbConn(txdb)
#     res <- dbGetQuery(con,
#                       paste0("SELECT value FROM metadata WHERE name='", name, "'"))[[1]]
#     if(!is.character(res))
#         stop("metadata table missing a value for '", name, "'")
#     res
# }
#
# .makePackageName <- function(mgdb){}
#
# .makeObjectName <- function(pkgName){
#     strs <- unlist(strsplit(pkgName, "\\."))
#     paste(c(strs[2:length(strs)],strs[1]), collapse="_")
# }
#
#
# .getMgDbVersion <- function(mgdb){
#     type <- .getMetaDataValue(mgdb,'Data source')
#     ## code specific to database source
# #     if(type=="UCSC"){
# #         version <- paste(.getMetaDataValue(txdb,'Genome'),"genome based on the",
# #                          .getMetaDataValue(txdb,'UCSC Table'), "table")
# #     }else if(type=="BioMart"){
# #         version <- .getMetaDataValue(txdb,'BioMart database version')
# #     }else{
# #         version <-  .getMetaDataValue(txdb,'Data source')
# #     }
# #    version
# }
#
# .getResourceURLInfo <- function(txdb){
#     mgdb <- .getMetaDataValue(mgdb,'Data source')
#     ## code specific to database source
# #     if(type=="UCSC" || type=="BioMart"){
# #         url <- .getMetaDataValue(txdb,'Resource URL')
# #     }else{
# #         url <- .getMetaDataValue(txdb,'Data source')
# #     }
#     url
# }
#
#
# makeMxDbPackage <- function(mgdb,
#                             version,
#                             maintainer,
#                             author,
#                             destDir=".",
#                             license="Artistic-2.0"){
#     ## every package has a name We will generate this according to a heuristic
#     pkgName <- .makePackageName(mgdb)
#     dbType <- .getMetaDataValue(mgdb,'Db type')
#
#     ## there should only be one template
#     template_path <- system.file("mgdb-template",package="metagenomicFeatures")
#     ## We need to define some symbols in order to have the
#     ## template filled out correctly.
#     symvals <- list(
#         PKGTITLE=paste("Annotation package for",dbType,
#                        "object(s)"),
#         PKGDESCRIPTION=paste("Exposes an annotation databases generated from",
#                              .getMetaDataValue(mgdb,'Data source'), "by exposing these as",dbType,
#                              "objects"),
#         PKGVERSION=version,
#         AUTHOR=author,
#         MAINTAINER=maintainer,
#         GFVERSION=.getMetaDataValue(mgdb,
#                                     'GenomicFeatures version at creation time'),
#         LIC=license,
#         DBTYPE= dbType,
#         PROVIDER=.getMetaDataValue(mgdb,'Data source'),
#         PROVIDERVERSION=.getMgDbVersion(mgdb),
#         RELEASEDATE= .getMetaDataValue(mgdb,'Creation time'),
#         SOURCEURL= .getResourceURLInfo(mgdb),
#         MGDBOBJNAME=pkgName
#     )
#     ## Should never happen
#     if (any(duplicated(names(symvals)))) {
#         str(symvals)
#         stop("'symvals' contains duplicated symbols")
#     }
#     ## All symvals should by single strings (non-NA)
#     is_OK <- sapply(symvals, isSingleString)
#     if (!all(is_OK)) {
#         bad_syms <- paste(names(is_OK)[!is_OK], collapse=", ")
#         stop("values for symbols ", bad_syms, " are not single strings")
#     }
#     createPackage(pkgname=pkgName,
#                   destinationDir=destDir,
#                   originDir=template_path,
#                   symbolValues=symvals)
#     ## then copy the contents of the database into the extdata dir
#     db_path <- file.path(destDir, pkgName, "inst", "extdata",
#                          paste(pkgName,"sqlite",sep="."))
#     ## will also need to move and save the sequence object as well
#     saveDb(mgdb, file=db_path)
# }
#
# # seperate functions for each database source, try to combine if possible
#
# makeMgDbPackageFromGreenGenes <- function(
#     version,
#     maintainer,
#     author,
#     destDir=".",
#     license="Artistic-2.0",
#     tablename,
#     url){
#         ## checks on the user provided argument formats
#         if(missing(version) || !isSingleString(version)){
#             stop("'version' must be supplied as a single element",
#                  " character vector.")}
#         if(missing(version) || !isSingleString(maintainer)){
#             stop("'maintainer' must be supplied as a single element",
#                  " character vector.")}
#         if(missing(version) || !isSingleString(author)){
#             stop("'author' must be supplied as a single element",
#                  " character vector.")}
#         if(!isSingleString(destDir)){
#             stop("'destDir' must be supplied as a single element",
#                  " character vector.")}
#         if(!isSingleString(license)){
#             stop("'license' must be supplied as a single element",
#                  " character vector.")}
#
#         ## Make the DB
#         mgdb <- makeMgDbGreenGenes() # need to provide arguments required to make the database
#             #function defined in makeMgDbGreenGenes.R
#
#         ## Make the Package
#         makeMgDbPackage(mgdb,
#                         version=version,
#                         maintainer=maintainer,
#                         author=author,
#                         destDir=destDir,
#                         license=license)
# }
#
# makeMgDbPackageFromSilva <- function(){}
#
# makeMgDbPackageFromRDP <- function(){}
