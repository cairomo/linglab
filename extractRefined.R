library(rpubmed)
library(RISmed)

parameter <- 'NOT (review[Publication Type] OR Bibliography [Publication Type] OR Editorial[Publication Type] OR Letter[Publication Type] OR Meta-analysis[Publication Type] OR News[Publication Type])'

mrconso <- read.csv("/Volumes/DISK_IMG\ 1/mrconso.csv", sep = "\t")
print("done reading\n")
mrconso_rownames <- c("cui", "lang", "term status", "lui", "str type", "sui", "ispref", "aui", "saui", "scui", "sdui", "source", "term type", "code", "str", "src restrict", "suppr", "cont view")
colnames(mrconso) <- mrconso_rownames

strokeCUI <- "C0038454"
strokePrefName <- "Cerebrovascular accident"
finalQuery <- strokePrefName
# numUniqueCUIs <- sum(duplicated(mrconso[["cui"]]) == FALSE)

isPreferred <- sapply(seq(1, nrow(mrconso), 1), function(i) {
#isPreferred <- sapply(seq(1, 1000, 1), function(i) { # for testing
    if ((mrconso[i,"lang"] == "ENG") &&
        (mrconso[i,"term status"] == "P") &&
        (mrconso[i,"str type"] == "PF") &&
        (mrconso[i,"ispref"] == "Y")) {
        return(as.character(mrconso[i,"str"]))
    } else {
        return("")
    }
})
print("created isPreferred bool array\n")
refined <- isPreferred[isPreferred != ""] # gets all non empty rows
write.csv(isPreferred, "isPreferred.csv")

frequency <- vector(length = NROW(refined))
for (i in 1:NROW(refined)) {
    finalQuery <- paste(finalQuery, "AND", refined[i], parameter, sep=" ")
    tempRes <- EUtilsSummary(finalQuery, type='esearch', db='pubmed')
    frequency[i] <- QueryCount(tempRes)
    #  if ((i %% 200) == 0) {
    #      print(refined[i,])
    #      print("\n")
    #  }
}

result <- cbind(refined, frequency)
write.csv(result, "result.csv")
#refined <- array(dim=(2*sum(isPreferred == TRUE)))
#dim(refined) <- c(sum(isPreferred == TRUE), 2)
#write.csv(refined, "refined.csv")