library(tidyverse)
library(rtweet)
library(tidytext)
library(stringr)
library(janitor)
library(extrafont)
library(patchwork)


# Create ggplot2 theme for Animal Crossing game
ac_theme <- theme_bw() + 
  theme(
  plot.background = element_rect(fill = "#8E6D35"), #brown
  #panel.grid.major = element_line(colour = "#F6D04C"), #yellow
  panel.grid.major = element_line(colour = "#E2BDD4"), #pink
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.ticks = element_line(colour = "darkgoldenrod1"), #red
  axis.text = element_text(colour = "darkgoldenrod1", size = rel(1.0), family = "FinkHeavy"), #red
  axis.title = element_text(colour = "white"),
  legend.background = element_rect(fill = "white", "gold2"),
  legend.title = element_text(colour = "forestgreen"), #9AD2E4 try blue
  legend.text = element_text(colour = "forestgreen"),
  legend.key = element_rect(fill = NA),
  strip.background = element_rect(fill = "gold1"),
  strip.text = element_text(colour = "white"),
  plot.title = element_text(colour = "white", hjust = 0.5, family = "FinkHeavy", size = 20),
  plot.subtitle = element_text(colour = "white", hjust = 0.5, family = "FinkHeavy"), #green
  plot.caption = element_text(color = "white", family = "FinkHeavy"),
)


# Getting tweets for Animal Crossing
tweets_ac <- search_tweets(q = '#AnimalCrossing', n = 18000, 
                           include_rts = FALSE, lang="en", parse = TRUE)



# Clean tweets
tweets_ac_clean <- janitor::remove_empty(tweets_ac, which = "cols")

# Tidy data
sub_tweets_ac <- tweets_ac_clean %>% 
  select(screen_name, user_id, created_at, status_id, text,display_text_width,
         retweet_count, favorite_count, hashtags, source, 
         media_type, location, followers_count, friends_count)


#write.csv(tweets_ac, "/Users/hazelkavili/Desktop/animal_crossing/animalcrossing_tweets/tweets_ac.csv")

# Source
p4 <- sub_tweets_ac %>% 
  count(source, sort = TRUE) %>% 
  mutate(percentages = n/sum(n)) %>% 
  head(n = 4) %>% 
  ggplot(aes(x = reorder(source, n), y = n)) +
  geom_col(fill="#D33934") +
  coord_flip() +
  labs(
    x = NULL, y = NULL,
    title = "Source of #AnimalCrossing tweets",
    subtitle = "",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) +
  ac_theme 


p3 <- tweets_ac %>% group_by(user_id, source) %>% 
  summarise(user_tweets = n()) %>% 
  ggplot(aes(x = user_tweets)) +
  geom_histogram(binwidth=1, fill = "#397F34") +
  labs(
    x = "Tweet counts", y = "",
    title = "Tweet Counts Histogram of users' #AnimalCrossing tweets",
    subtitle = "",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) +
  ac_theme 



# Hashtags
ac_tidy_hashtags <- tweets_ac %>% unnest(hashtags) 

ac_tidy_hashtags <- ac_tidy_hashtags %>%
  count(hashtags, sort = TRUE) %>% top_n(n = 10, wt = n) 


p <- ac_tidy_hashtags %>%  filter(hashtags != "AnimalCrossing") %>% filter(hashtags != "どうぶつの森")

ggplot(data = p, aes(x = reorder(hashtags, n), y = n)) + 
  geom_col(fill="#D33934") +
  coord_flip() +
  labs(
    x = NULL, y = NULL,
    title = "Hashtags used in #AnimalCrossing tweets",
    subtitle = "",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) +
  ac_theme 
 

#avg display_text_width on different source
text_width_by_source <- sub_tweets_ac %>% 
  mutate(clean_texts = gsub("https.*","", text)) %>% 
  group_by(source) %>% 
  mutate(total_chars = (nchar(clean_texts))-15) %>% 
  group_by(source) %>% 
  summarise(unique_users = n_distinct(user_id),
    avg_text_width = mean(total_chars))


p1 <-  text_width_by_source %>% filter(unique_users > 20) %>% 
  ggplot(aes(x = reorder(source, unique_users), y = unique_users)) +
  geom_col(fill="#9AD2E4") +
  coord_flip() +
  labs(
    x = NULL, y = NULL,
    title = "Number of unique users who sent #AnimalCrossing tweets",
    subtitle = "",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) +
  ac_theme 

p2 <- text_width_by_source %>% filter(unique_users > 20) %>%
  ggplot(aes(x = reorder(source, avg_text_width), y = avg_text_width)) +
  geom_col(fill="#9AD2E4") +
  coord_flip() +
  labs(
    x = NULL, y = NULL,
    title = "Average text width for #AnimalCrossing tweets",
    subtitle = "",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) +
  ac_theme 

  
  
(p1 / p3)
(p2 / p4)
  
#Most fav tweet
# favourited
mostFavourited <- sub_tweets_ac %>% arrange(desc(favorite_count)) %>%
  select(text) %>% head(1) 

getURLinsideFavTweet <- gsub(".*(https://)", "https://",mostFavourited$text)

browseURL(getURLinsideFavTweet)
  



  






