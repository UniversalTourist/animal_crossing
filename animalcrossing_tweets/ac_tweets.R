library(tidyverse)
library(rtweet)
library(tidytext)
library(stringr)
library(janitor)


# Getting tweets
tweets_ac <- search_tweets(q = '#AnimalCrossing', n = 18000, 
                           include_rts = FALSE, parse = TRUE)


#create ggplot theme
ac_theme <- theme_bw() + 
  theme(
  plot.background = element_rect(fill = "#8E6D35"), #brown
  panel.grid.major = element_line(colour = "#F6D04C"), #yellow
  panel.grid.minor = element_blank(),
  axis.ticks = element_line(colour = "darkgoldenrod1"), #red
  axis.text = element_text(colour = "darkgoldenrod1", size = rel(1.0)), #red
  #axis.title = element_text(colour = "orange1"),
  legend.background = element_rect(fill = NA, "gold2"),
  legend.title = element_text(colour = "forestgreen"),
  legend.text = element_text(colour = "#459948"),
  legend.key = element_rect(fill = NA),
  strip.background = element_rect(fill = "gold1"),
  strip.text = element_text(colour = "white"),
  plot.title = element_text(colour = "darkgoldenrod1", hjust = 0.5),
  plot.subtitle = element_text(colour = "white", hjust = 0.5), #green
  plot.caption = element_text(color = "white"),
)

tweets_ac %>%
  ts_plot("mins") +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #AnimalCrossing tweets",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) +
  ac_theme
