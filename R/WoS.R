#####################################################################
#' Apply OECD categories to web of science subject area
#'
#' @param df A dataframe of papers
#' @return A dataframe of WoS records with OECD fields
mergeOECD <- function(df) {
  df <- df %>%
    dplyr::mutate(WC = as.character(WC)) %>%
    dplyr::mutate(WC = gsub("GREEN & SUSTAINABLE SCIENCE & TECHNOLOGY; ","",WC)) %>%
    dplyr::mutate(WC = gsub("   "," ",WC)) %>%
    dplyr::mutate(WC = gsub("\n","",WC)) %>%
    tidyr::separate(WC,c("WC"),"; ",extra="drop")

  df <- df %>%
    dplyr::mutate(UWC = toupper(WC)) %>%
    dplyr::left_join(OECDCats,by=c("UWC" = "Web.of.Science.Subject.Area"))

  return(df)
}


#####################################################################
## Read data from web of science
#' Read data from Web of Science (WoS)
#'
#' @param p A path to a WoS file or directory of WoS files
#' @return A dataframe of WoS records
readWoS <- function(p) {
  require(assertthat)
  require(dplyr)
  require(parallel)
  # Calculate the number of cores
  no_cores <- detectCores() - 1

  # Initiate cluster

  if(assertthat::is.dir(p)) {
    files <- list.files(p,pattern=".txt",full.names=T)
    d <- T
  } else {
    files <- p
    d <- F
  }
  pattern <- "(\n[A-Z]{1}[A-Z19]{1})"
  splitr <- function(x) {
    f <- substr(x,1,2)
    data <- substring(x,4)
    x <- list()
    x[f] <- data
    return(x)
  }
  combinefs <- function(x) {
    x <- readChar(x,file.info(x)$size)
    return(x)
  }

  ppaste <- function(x) {
    x <- paste0("PT",strsplit(x,"\nPT",x,fixed=TRUE)[[1]])
    x <- strsplit(gsub(pattern,"~~\\1",x),"~~\n",fixed=TRUE)
    x[[1]] <- NULL
    return(x)
  }

  if (no_cores > 1) {
    cl <- makeCluster(no_cores)
    x <- parLapply(cl,files,combinefs)
    f <- parLapply(cl,x,ppaste)
    f <- do.call(list, unlist(f, recursive=FALSE))
    f <- parLapply(cl,f,splitr)
    stopCluster(cl)
  } else {
    x <- lapply(files,combinefs)
    f <- lapply(x,ppaste)
    f <- do.call(list, unlist(f, recursive=FALSE))
    f <- lapply(f,splitr)
  }

  fsdf <- rbind_all(f)


  fsdf$EF <- NULL
  fsdf$PY <- as.numeric(fsdf$PY)
  return(fsdf)
}

WoSCitations <- function(p) {
  require(assertthat)
  require(dplyr)
  if(assertthat::is.dir(p)) {
    files <- list.files(p,pattern=".txt",full.names=T)
    d <- T
  } else {
    files <- p
    d <- F
  }
  pattern <- "(\n[A-Z]{2})"
  splitr <- function(x) {
    f <- substr(x,1,2)
    data <- substring(x,4)
    x <- list()
    x[f] <- data
    return(x)
  }
}
