#####################################################################
#' Turn a dataframe of papers into a corpus
#'
#' @param df A dataframe of papers
#' @return A corpus of words, punct removed and stemmed
#' @export
#' @import tm
#' @import dplyr
corporate <- function(df,col="AB") {

  ignoreWords <- c("the","however","this","and")

  corpus <- tm::Corpus(tm::VectorSource(df[[col]])) %>%
    tm::tm_map(tm::PlainTextDocument) %>%
    tm::tm_map(tm::removePunctuation) %>%
    tm::tm_map(tm::removeWords,tm::stopwords()) %>%
    #tm_map(removeNumbers(lazy=TRUE)) %>%
    tm::tm_map(tm::removeWords,ignoreWords) %>%
    tm::tm_map(tm::stemDocument)

  return(corpus)
}


#####################################################################
#' Turn a corpus into a document term matrix
#'
#' @param df A dataframe of papers
#' @return A corpus of words, punct removed and stemmed
#' @export
#' @import tm
#' @import dplyr
#' @import slam
makeDTM <- function(corpus,sparsity,rnames,cols,rows) {
  dtm <- tm::DocumentTermMatrix( # make a document term matrix
    corpus,
    control = list(minWordLength = 3,tm::removeNumbers)
  )

  row.names(dtm) <- rnames

  dtm <- tm::removeSparseTerms(dtm, sparsity)
  ## make some adjustments to the dtm

  print(dim(dtm))

  term_tfidf <-
    tapply(dtm$v/slam::row_sums(dtm)[dtm$i],dtm$j,mean) *
    log2(tm::nDocs(dtm)/slam::col_sums(dtm > 0))

  print(summary(term_tfidf))

  dtm <- dtm[,term_tfidf >= cols] # slightly bigger than median

  removed_docs <- row.names(dtm[slam::row_sums(dtm)<=rows,])

  dtm <- dtm[slam::row_sums(dtm)>rows,]

  print(summary(slam::col_sums(dtm)))

  print(dim(dtm))

  x <- list(dtm=dtm,removed=removed_docs)

  return(x)
}


#####################################################################
#' Make sure the corpus reflects the dtm after working on it
#'
#' @param dtm a document term matrix
#' @return A corpus of words that reflect the terms in the dtm
#' @export
#' @import tm
#'
refresh_corp <- function(dtm) {
  dtm2list <- apply(dtm, 1, function(x) {
    paste(rep(names(x), x), collapse=" ")
  })
  ## convert back to a Corpus
  corp <- VCorpus(VectorSource(dtm2list))
  return(corp)
}


#####################################################################
#' visualise the topic model
#'
#' @param model a topic model
#' @param corpus a corpus
#' @dtm a document term matrix
#'
#' @export
#'
#'
visualise <- function(model,corpus,dtm) {
  json <- topicmodels_json_ldavis(model,corpus,dtm)
  modelName <- deparse(substitute(model))
  unlink(modelName, recursive=TRUE)
  serVis(json, out.dir = modelName, open.browser = F)
  save(model,file=paste0(modelName,"/model_output.RData"))
}
