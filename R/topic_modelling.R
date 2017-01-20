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
#' @param corpus - A document corpus
#' @param sparsity - An integer between 0 and 1. Lower values remove more highly common and highly uncommon terms
#' @param rnames - a vector of document identifiers (usually papers$UT)
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

  if (sparsity < 1) {
    dtm <- tm::removeSparseTerms(dtm, sparsity)
  }

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
#' @param dtm a document term matrix
#' @export
#' @import servr
visualise <- function(model,corpus,dtm) {
  cwd <- getwd()
  username <- strsplit(strsplit(cwd,"/home/")[[1]][2],"/")[[1]][1]
  json <- topicmodels_json_ldavis(model,corpus,dtm)
  modelName <- deparse(substitute(model))
  unlink(modelName, recursive=TRUE)
  serVis(json, out.dir = modelName, open.browser = F)
  save(model,file=paste0(modelName,"/model_output.RData"))
  dir <- paste0('/var/www/html/',username,'/')
  dir.create(dir,showWarnings = F)
  path <- paste0(dir,modelName)
  unlink(path,recursive=TRUE)
  serVis(json, out.dir = path, open.browser = F)
}

#####################################################################
#' Produce a topic correlation graph file
#'
#' @param model a topic model
#' @param corpus a corpus
#' @param dtm a document term matrix
#' @export
#' @import igraph
topCors <- function(model) {
  gamma <- as.data.frame(model@gamma)
  topic_names <- paste0("{",terms(model,10)[1,],", ",terms(model,10)[2,],", ",terms(model,10)[3,],"}")

  names(gamma) <- topic_names
  cors <- cor(gamma[,])
  cors[lower.tri(cors,diag=TRUE)] <- 0
  cors[cors < 0] <- 0
  colnames(cors) <- topic_names

  g <- as.undirected(graph.adjacency(
    cors,weighted=TRUE,mode="upper"
  ))

  layout1 <- layout.fruchterman.reingold(g, niter=500)

  b1 <- degree(g)

  V(g)$label.cex <-  b1 * 2  / max(b1) # label text size
  V(g)$size <-  b1 * 30 / max(b1)        # node size

  return(g)
}
