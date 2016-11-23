#####################################################################
#' Plot a summary of the results from the bilbiometric analysis
#'
#' @author Jerome Hilaire
#'
#' @param df a dataframe of papers
#' @export
#' @import bibliometrix
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
plot_biblioAnalysis <- function(df) {

  # http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
      print(plots[[1]])

    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }

  tmp <- biblioAnalysis(df)

  tmp_au <- as.data.frame(tmp$Authors) %>% top_n(20)
  p_au = ggplot(data= tmp_au %>% mutate(AU=factor(AU, levels=rev(tmp_au$AU), ordered=TRUE))) +
    geom_bar(aes(x=AU, y=Freq), stat = "identity", position = "stack", fill="blue") +
    coord_flip()+
    theme_bw() +
    #theme(axis.text.x=element_text(angle=90,hjust=1)) +
    ggtitle("Top 20 Authors") + xlab("Author") + ylab("Frequency")

  tmp_so <- as.data.frame(tmp$Sources) %>% top_n(20) %>% mutate(SO=abbreviate(SO,10))
  p_so = ggplot(data=tmp_so %>% mutate(SO=factor(SO, levels=rev(tmp_so$SO), ordered=TRUE))) +
    geom_bar(aes(x=SO, y=Freq), stat = "identity", position = "stack", fill="blue") +
    coord_flip()+
    theme_bw() +
    #theme(axis.text.x=element_text(angle=90,hjust=1)) +
    ggtitle("Top 20 Sources") + xlab("Source") + ylab("Frequency")

  p_py = ggplot(data=as.data.frame(table(tmp$Years), stringsAsFactors = FALSE) %>% mutate(Var1=as.numeric(Var1))) +
    geom_line(aes(x=Var1, y=Freq), color="blue") +
    theme_bw() +
    #theme(axis.text.x=element_text(angle=90,hjust=1)) +
    ggtitle("Number of publications per year") + xlab("Years") + ylab("Frequency")

  tmp_co <- as.data.frame(tmp$Countries) %>% top_n(20)
  p_co = ggplot(data=tmp_co %>% mutate(CO=factor(CO, levels=rev(tmp_co$CO), ordered=TRUE))) +
    geom_bar(aes(x=CO, y=Freq), stat = "identity", position = "stack", fill="blue") +
    coord_flip()+
    theme_bw() +
    #theme(axis.text.x=element_text(angle=90,hjust=1)) +
    ggtitle("Top 20 Countries") + xlab("Country") + ylab("Frequency")

  multiplot(p_au, p_so, p_py, p_co, cols = 2)

  return()

}

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
