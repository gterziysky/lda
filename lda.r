library(lda)
library(rjson)
library(tm)

# switch(tolower(Sys.info()["sysname"]),
#        windows = setwd("~/../Desktop/lda"),
#        linux = setwd("~/Desktop/lda"),
#        stop("unknown OS"))
setwd("yourworkingdirectory")
source("BuildDocSet.r")
source("BuildVocab.r")

runFromScratch <- T
vocab <- 0
processedDocs <- 0

if (runFromScratch) {
  # sample of documents
  docs <- getRawDocSet(fileName = "corpus_sample.txt")
  vocab <- buildVocab(docs)
  
  processedDocs <- buildDocSet(docs, vocab)
  
  # convert matrices from numeric to integer
  processedDocs <- lapply(processedDocs, function(m){
    m2 <- apply (m, c (1, 2), function (x) {
      (as.integer(x))
    })
    (m2)
  })
  
  saveRDS(vocab,"ldaVocabExclStopWords.Rds")
  saveRDS(processedDocs,"ldaCorpusNoStopWords.Rds")
  
} else {
  vocab <- readRDS("ldaVocabExclStopWords.Rds")
  processedDocs <- readRDS("ldaCorpusNoStopWords.Rds")
}

lda <- lda.collapsed.gibbs.sampler(documents = processedDocs,
                            K = 10, # fit 10 topics
                            vocab,
                            num.iterations = 25,
                            alpha = 0.1,
                            eta = 0.1,
                            compute.log.likelihood = FALSE)

# normalise topic distributions over words
i <- 1
topics <- apply(lda$topics, 1, function(x) {
  x <- x / lda$topic_sums[i]
  i <- i+1
  (x)
})

words <- rownames(topics)
topicList <- list()
for (i in 1:ncol(topics)) {
  topic <- data.frame(prob = topics[, i])
  topic$word <- words
  topic <- topic[order(-topic$prob), ]
  rownames(topic) <- NULL
  topicList[[i]] <- topic
}

docList <- list()
# document 1, distributions
for (i in 1:ncol(lda$document_sums)) {
  doc <- data.frame(prob = lda$document_sums[, i])
  # normalise per document topic distributions 
  if (sum(lda$document_sums[, i]) != 0) {
    doc$prob <- doc$prob / sum(lda$document_sums[, i])
  }
  doc$topic <- c("topic1", "topic2", "topic3", "topic4", "topic5",
                 "topic6", "topic7", "topic8", "topic9", "topic10")
  doc <- doc[order(-doc$prob, doc$topic), ]
  rownames(doc) <- NULL
  docList[[i]] <- doc
}

# per document topic distribution
View(docList[[1]])
# per topic word distribution
View(topicList[[1]])
