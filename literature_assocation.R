# 1. from umls, get synonyms, keywords, terms that are 
#       semantically related to [target]
# 2. search all synonyms of [target] + "risk prediction" in literature database
# 3. mesh terms + keywords from results are the feature list
# 4. for every feature, do co-occurrence analysis with all synonyms of [target]

library(rpubmed)
library(RISmed)

parameter <- 'NOT (review[Publication Type] OR Bibliography [Publication Type] OR Editorial[Publication Type] OR Letter[Publication Type] OR Meta-analysis[Publication Type] OR News[Publication Type])'
keywords <- 'AND (risk prediction model[Title/Abstract] OR predictive model[Title/Abstract] OR predictive equation[Title/Abstract] OR prediction model[Title/Abstract] OR risk calculator[Title/Abstract] OR prediction rule[Title/Abstract] OR risk model[Title/Abstract] OR statistical model[Title/Abstract] OR cox model[Title/Abstract] OR multivariable[- Title/Abstract])'
mrconso <- read.csv("/Volumes/DISK_IMG\ 1/mrconso.csv", sep = "\t")
print("done reading\n")
mrconso_rownames <- c("cui", "lang", "term status", "lui", "str type", "sui", "ispref", "aui", "saui", "scui", "sdui", "source", "term type", "code", "str", "src restrict", "suppr", "cont view")
colnames(mrconso) <- mrconso_rownames

strokeCUI <- "C0038454"
strokePrefName <- "Cerebrovascular accident"
finalQuery <- strokePrefName
# numUniqueCUIs <- sum(duplicated(mrconso[["cui"]]) == FALSE)

synonyms <- sapply(seq(1, nrow(mrconso), 1), function(i) {
    #synonyms <- sapply(seq(1, 1000, 1), function(i) { # for testing
    if ((mrconso[i,"cui"] == strokeCUI) &&
        ((mrconso[i,"str type"] == "PF") || (mrconso[i,"str type"] == "SY"))
        ) {
        return(as.character(mrconso[i,"str"]))
    } else {
        return("")
    }
})
print("created synonyms bool array\n")
refined <- synonyms[synonyms != ""] # gets all non empty rows
write.csv(synonyms, "synonyms.csv")

frequency <- vector(length = NROW(refined))
allHeadings <- vector(length = NROW(refined))

for (i in 1:NROW(refined)) {
    finalQuery <- paste(refined[i], keywords, parameter, sep=" ")
    tempRes <- EUtilsSummary(finalQuery, type='esearch', db='pubmed')
    frequency[i] <- QueryCount(tempRes)
    fetch <- EUtilsGet(tempRes)
    mesh <- Mesh(fetch)
    allHeadings[i] <- mesh
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