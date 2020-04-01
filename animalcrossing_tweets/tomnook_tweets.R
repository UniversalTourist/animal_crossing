# Getting tweets for Animal Crossing
tweets_tom_nook <- search_tweets(q = '#TomNook', n = 18000, 
                           include_rts = FALSE, lang="en", parse = TRUE)



# Clean tweets
tweets_tom_nook_clean <- janitor::remove_empty(tweets_tom_nook, which = "cols")

# Tidy data
sub_tweets_tom_nook_clean <- tweets_tom_nook_clean %>% 
  select(screen_name, user_id, created_at, status_id, text,display_text_width,
         retweet_count, favorite_count, hashtags, source, 
         media_type, location, followers_count, friends_count)
