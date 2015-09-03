## script used to generate mgQuery.rdata
library(msd16s)
library(ShortRead)
n = 500
ord = order(rowSums(msd16s),decreasing=TRUE)[1:n]
centers = as.character(fData(msd16s[ord,])$clusterCenter)
query = DNAStringSet(centers)
otu_ids = rownames(fData(msd16s[ord,]))

# If I don't include a named DNAStringObject I get errors when running annotate:
# Error in data.frame(query_id = names(query_subset), Keys = db_subset,  :
#  arguments imply differing number of rows: 0, 30
names(query) = otu_ids
id = BStringSet(otu_ids)
query = ShortRead(query,id)

tmpd = tempdir()
fl = paste(tmpd,"query.rdata",sep="")
save(query,file=fl)
