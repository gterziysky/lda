buildVocab <- function (docs = NULL) {
  if (is.null(docs)) {
    docs <- getRawDocSet()
  }
  # convert to lower case, insure uniqueness and sort in ascending order
  vocab <- sort(unique(tolower(unlist(docs, recursive = F, use.names = F))))
  return(vocab)
}
