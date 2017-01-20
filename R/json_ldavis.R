#####################################################################
# SOURCE: http://www.r-bloggers.com/a-link-between-topicmodels-lda-and-ldavis/
#' Output topic model results in json format
#'
#' @param fitted a topic model
#' @param corpus a corpus
#' @param doc_term a document term matrix
#' @export
#' @import servr
#' @import dplyr
#' @import stringi
#' @import tm
#' @import LDAvis
topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  # Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)

  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }

  temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)

  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)

  return(json_lda)
}



topicmodels_tf <- function(fitted, corpus, doc_term){
  # Required packages
  library(topicmodels)
  library(dplyr)
  library(stringi)
  library(tm)
  library(LDAvis)

  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }

  temp_frequency <- inspect(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)

  term.frequency = freq_matrix$Freq

  return(term.frequency)
}
