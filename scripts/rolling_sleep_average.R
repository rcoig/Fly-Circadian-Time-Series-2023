calculate_rolling_average <- function(df, minutes_window = 5) {
  library(dplyr)
  
  df <- df %>%
    arrange(Monitor,Tube, Minute) %>%
    group_by(Monitor,Tube) %>%
    mutate(Rolling_Average = zoo::rollmean(Summary_Ct, k = minutes_window, fill = NA)) %>%
    mutate(Sleeping = ifelse(is.na(Rolling_Average) | Rolling_Average == 0, 1, 0)) %>%
    fill(Sleeping, .direction = "up") %>%
    ungroup()
  
  return(df)
}