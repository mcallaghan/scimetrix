#####################################################################
#' Plot numbers of papers by variable and year
#'
#' @param df a dataframe of papers
#' @param field a string describing the variable to pass to the bar fill
#' @param n the number of categories to display (defaults to "all")
#' @param summed is the data already summed? (default F)
#' @param graph return a graph object (default FALSE) or data
#' @param bSize set the base size of the text
#' @return Either a data frame of year and category sums, or a graph object
#' @export
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
paperNumbers <- function(df,field,n="all",summed=F,graph=F,bSize=12) {
  df[,"z"] <- df[,field]

  if (summed==F) {
    rankings <- df %>%
      group_by(z) %>%
      summarise(n = length(z)) %>%
      arrange(-n)
    if(n=="all") {
      n = length(df$z)
    }
  } else {
    rankings <- df %>%
      group_by(z) %>%
      summarise(n = sum(n)) %>%
      arrange(-n)
    if(n=="all") {
      n = length(unique(df$z))
    }
  }


  df$z <- factor(df$z,levels=rankings$z[1:n])


  rankings$z[1:n]

  if (summed==F) {
    p <- ggplot(
      filter(df,as.character(z) %in% as.character(rankings$z[1:n]))
    )
    p <- p +
      geom_bar(
        aes(PY,fill=z),
        colour="grey22"
      )
  } else {
    df <- df %>%
      arrange(z)
    p <- ggplot(
      df
    )
    p <- p +
      geom_bar(
        aes(PY,n,fill=z),
        colour="grey22",
        stat="identity"
      )
  }
  p <- p +
    scale_fill_brewer(palette="Set3",guide=guide_legend(reverse=T)) +
    theme_classic(base_size = bSize) +
    theme(
      panel.grid.major.y=element_line(size=0.2,colour="grey22"),
      panel.border=element_rect(size=0.2,colour="grey22",fill=NA),
      legend.position=c(0.01,0.99),
      legend.justification=c(0,1)
    ) +
    labs(x="Year",y="Number of Publications",fill="Subject Area")

  print(p)

  if (graph==T) {
    return(p)
  }

  sums <- df %>%
    group_by(z,PY) %>%
    summarise(
      n=length(PY)
    ) %>%
    spread(z,n)

  return(sums)
}



#####################################################################
#' Plot shares of papers by variable and year
#'
#' @param df a dataframe of papers
#' @param field a string describing the variable to pass to the bar fill
#' @param n the number of categories to display (defaults to "all")
#' @param pType plot type, either "bar" or "line"
#' @param bSize set the base size of the text
#' @return A dataframe of the summed results
#' @export
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
paperShares <- function(df,field,n="all",pType="bar",bSize=12) {
  df[,"z"] <- df[,field]

  rankings <- df %>%
    group_by(z) %>%
    summarise(n = length(z)) %>%
    arrange(-n)

  if(n=="all") {
    n = length(df$z)
  }

  df$z <- factor(df$z,levels=rankings$z[1:n])

  df <- filter(df,!is.na(z))

  if(pType!="bar") {
    d <- 1
  } else {
    d <- 5
  }

  shares <- df %>%
    filter(PY %% d==0) %>%
    group_by(PY,z) %>%
    summarise(
      n = length(z)
    )

  ytotals <- shares %>%
    group_by(PY) %>%
    summarise(
      total = sum(n)
    )

  shares <- shares %>%
    left_join(ytotals) %>%
    mutate(share = n/total*100)

  bp <- ggplot(shares) +
    geom_bar(
      aes(PY,y=share,fill=z),
      colour="grey22",
      stat="identity",
      #position= position_dodge(4)
      position = "dodge"
    ) +
    scale_fill_brewer(palette="Spectral",name="Subject Area") +
    theme_classic(base_size = bSize) +
    theme(
      panel.grid.major.y=element_line(size=0.2,colour="grey22"),
      panel.border=element_rect(size=0.2,colour="grey22",fill=NA)
    )
  labs(x="Year",y="Publication Share")

  lp <- ggplot(shares) +
    geom_line(
      aes(PY,y=share,colour=z),
      size=1.5
    ) +
    scale_colour_brewer(palette="Spectral",name="Subject Area") +
    theme_classic(base_size = bSize) +
    theme(
      panel.grid.major.y=element_line(size=0.2,colour="grey22"),
      panel.border=element_rect(size=0.2,colour="grey22",fill=NA)
    )
  labs(x="Year",y="Publication Share")

  if(pType=="bar") {
    print(bp)
  } else {
    print(lp)
  }
  return(shares)
}


#####################################################################
#' Calculate and plot keyword sums
#'
#' @param df a dataframe of papers
#' @param n the number of kewords to display (defaults to 100)
#' @param bSize set the base size of the text
#' @return A dataframe of the summed results
#' @export
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#'
sumKeywords <- function(df,n=100,bSize=12) {
  keywords <- data.frame(keyword = unlist(lapply(df$ID,strsplit,";"))) %>%
    group_by(keyword) %>%
    summarise(n=length(keyword)) %>%
    filter(!is.na(keyword)) %>%
    arrange(-n) %>%
    head(n)
  keywords$keyword = factor(keywords$keyword,levels=rev(keywords$keyword))
  ggplot(keywords) +
    geom_bar(
      aes(keyword,n),
      stat="identity"
    ) +
    coord_flip() +
    theme_bw()
}
