
#####################################################################
#' Apply OECD categories to web of science subject area
#'
#' @param df A dataframe of papers
#' @param field The field to group growth rates by
#' @param y1 The year to start from
#' @param y2 The year to finish
#' @param total Is the df already summarised (default=F)
#' @return A table of growth rates by field
#' @export
#' @import dplyr
#' @import tidyr
#' @import slam
gRate <- function(df,field,y1,y2,total=F) {
  require(slam)
  df[,"z"] <- df[,field]
  if(total==F) {
    df <- df %>%
      filter(PY >=y1 & PY <=y2 & PY%%5==0 & !is.na(z)) %>%
      group_by(z,PY) %>%
      summarise(n=length(z)) %>%
      spread(z,n,fill=0) %>%
      mutate(Total=row_sums(.[-1])) %>%
      gather(z,n,-PY) %>%
      group_by(z)
  } else {
    df <- df %>%
      filter(PY >=y1 & PY <=y2 & PY%%5==0 & !is.na(z))
  }


  totals <- df %>%
    mutate(
      v1 = n[which.min(PY)],
      v2 = n[which.max(PY)],
      Growth = round(((v2/v1)^(1/(y2-y1))-1)*100) ,
      period=paste0("Total: ",y1,"-",y2)
    ) %>%
    select(period,z,Growth)

  df_sum <- df %>%
    mutate(
      change = n- lag(n),
      Growth = round(((n/lag(n))^(1/(PY-lag(PY)))-1)*100),
      period = paste0(lag(PY),"-",PY)
    ) %>%
    filter(!is.na(change)) %>%
    select(period,z,Growth)

  totals <- unique(totals)

  df_merge <- rbind(df_sum,totals) %>%
    spread(z,Growth)

  return(df_merge)
}
