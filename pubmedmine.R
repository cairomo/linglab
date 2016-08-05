library(rpubmed)
library(RISmed)
query <- 'inpatient readmission[Title] NOT (review[Publication Type] OR Bibliography [Publication Type] OR Editorial[Publication Type] OR Letter[Publication Type] OR Meta-analysis[Publication Type] OR News[Publication Type])'
res <- EUtilsSummary(query, type='esearch', db='pubmed')
# ids <- QueryId(res)
# metadata <- fetch_in_chunks(ids, chunk_size=500, delay = 0)
# numArticles <- QueryCount(res)
# abstracts <- array(0, dim = c(4, numArticles))
# for (i in 1:numArticles) { 
	# try(abstracts[1,i] <- metadata[i][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Abstract"]][["AbstractText"]][["text"]])
	# try(abstracts[2,i] <- metadata[i][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Abstract"]][2][["AbstractText"]][["text"]])
	# try(abstracts[3,i] <- metadata[i][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Abstract"]][3][["AbstractText"]][["text"]])
	# try(abstracts[4,i] <- metadata[i][["PubmedArticle"]][["MedlineCitation"]][["Article"]][["Abstract"]][4][["AbstractText"]][["text"]]) 
# }
fetch <- EUtilsGet(res)
mesh <- Mesh(fetch)
numHeadings <- 0
for (i in 1:(5*length(mesh))) { 
	if (!is.null(dim(mesh[[as.character(i)]]))) {
		numHeadings <- numHeadings + try(dim(mesh[[as.character(i)]])[1])
	}
}

headings <- array(dim=numHeadings)
counter <- 1
for (i in 1:numHeadings) {
	if (!is.null(dim(mesh[[as.character(i)]]))) {
		for (j in 1:dim(mesh[[as.character(i)]])[1]) {
			headings[counter] <- as.character(mesh[[as.character(i)]][j, "Heading"])
			counter <- counter + 1
		}
	}
}

wordsInDoc <- 100
frequencyOfWord <- 3 # num times target word appears in this doc
tf <- frequencyOfWord / wordInDoc
numDocuments <- 10000000 # 10 million
docFrequency <- 1000 # num docs that contain target word
idf <- log(numDocuments / docFrequency)
tfidf <- tf * idf
# remove duplicate headings
headings <- unique(headings)
# corresponding array of frequencies of each heading
frequency <- array(dim=length(headings))

for (i in 1:length(headings)) {
	tempQuery <- paste(query, " AND ", headings[i], "[Title/Abstract]", sep="")
	tempRes <- EUtilsSummary(tempQuery, type='esearch', db='pubmed')
	frequency[i] <- QueryCount(tempRes)
}

result <- cbind(headings, frequency)
write.csv(result, "inpatient_readmission.csv")
# write.csv(abstracts, "abstracts.csv")