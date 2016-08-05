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

isPreferred <- lapply(seq(1, nrow(mrconso), 1), function(i) {
    if ((mrconso[i,"lang"] == "ENG") &&
        (mrconso[i,"term status"] == "P") &&
        (mrconso[i,"str type"] == "PF") &&
        (mrconso[i,"ispref"] == "Y")) {
           TRUE
    } else {
        FALSE
    }
})

print("created isPreferred bool array\n")

refined <- array(dim=(2*sum(isPreferred == TRUE)))
dim(refined) <- c(sum(isPreferred == TRUE), 2)

index <- 1
for (i in 1:nrow(refined)) {
    #if (isPreferred[i] == TRUE) {
        refined[index,] <- c(as.character(mrconso[i, "cui"]), as.character(mrconso[i, "str"]))
        index <- index + 1
       # if ((index %% 20) == 0) {
       #     write.csv(refined[(index-20):index,], "temporary.csv")
       # }
    #}
}

print("extracted qualified cuis and names from mrconso\n")

frequency <- array(dim=nrow(refined))
counter <- 1

for (i in 1:nrow(refined)) {
    finalQuery <- paste(finalQuery, "AND", refined[i], parameter, sep=" ")
    tempRes <- EUtilsSummary(finalQuery, type='esearch', db='pubmed')
    frequency[counter] <- QueryCount(tempRes)
  #  if ((i %% 200) == 0) {
  #      print(refined[i,])
  #      print("\n")
  #  }
    counter <- counter + 1
}

result <- cbind(refined, frequency)
write.csv(refined, "refined.csv")
write.csv(frequency, "frequency.csv")
write.csv(result, "cva.csv")