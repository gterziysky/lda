buildDocSet <- function (docs = NULL, vocab = NULL, verbose = T) {
  # prepares the document to be used in LDA
  # returns a list of matrices (for each ddocument in docs) where every matrix
  # has 2 columns - a word_id and a word_frequency

  if(is.null(docs)) {
    docs <- getRawDocSet()
  }
  if(is.null(vocab)) {
    vocab <- buildVocab()
  }
  
  v <- vector(mode = "list", length = length(vocab))
  names(v) <- vocab
  
  i <- 1
  for (n in names(v)) {
    v[[n]] <- i
    i <- i + 1
  }
  
  res <- list()
  i <- 1
  totalDocCount <- length(docs)
  checkpoint = as.integer(totalDocCount %/% 10)
  for (doc in docs) {
    if (verbose == T && ((i %% checkpoint == 0) || i == 1)) {
      print(paste("processed", i, "out of", totalDocCount))
    }
    
    wordsInDoc <- unique(doc)
    m <- matrix(0L, nrow = 2, ncol = length(wordsInDoc))
    wordFreqs <- table(doc)
    j <- 1
    for (word in wordsInDoc) {
      # index of word in vocab
      # a 0-based index for the j-th word in document i.
      # That is, this should be an index - 1 into vocab.
      m[1, j] <- v[[word]] - 1
      # frequency of word in doc
      m[2, j] <- wordFreqs[word]
      j <- j + 1
    }
    
    res[[i]] <- m
    i <- i + 1
  }
  return(res)
}

getRawDocSet <- function (fileName) {
  res <- fromJSON(file = fileName)
  articles <- vector(mode = "character", length = length(res$news))
  for (i in seq_along(res$news)) {
    # replace whitespace characters with space
    txt <- gsub(pattern = "[\\s]", replacement = " ", x = res$news[[i]]$body, perl = T)
    # get only alpha characters
    articles[i] <- gsub(pattern = "[^a-zA-Z\\s]", replacement = "", x = txt, perl = T)
  }
  
  # docs is a list of character vectors which contain the words for each article
  docs <- lapply(articles, function(artTxt) {
    # split string into a vector of words
    txt <- strsplit(x = artTxt, split = "\\s", perl = T)[[1]]
    # convert to lowercase
    txt <- tolower(txt[txt != ""])
    # strip stop english words and return
    return(txt[!(txt %in% stopwords(kind = "en"))])
  })
  
  return(docs)
}
