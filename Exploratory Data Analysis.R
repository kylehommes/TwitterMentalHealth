# Wordclouds were made using frequency dataframes for each of the
# hashtags using the wordcloud2 package
wordcloud2(mhafreq.df)
wordcloud2(afreq.df)
wordcloud2(pfreq.df)
wordcloud2(sfreq.df)
wordcloud2(dfreq.df)
wordcloud2(totfreq.df)
# A word cloud was also created with the tradition wordcloud package for
# the mentalhealthawareness hashtag
wordcloud(mhafreq.df$word,mhafreq.df$freq,max.words=100,
  colors=brewer.pal(8,"Dark2"),scale=c(3,0.5),random.order=F)
# Plot of word frequency per hashtag
tot_word_freq <- tidytweet %>% count(word, hashtag, sort = TRUE) %>% 
  group_by(hashtag) %>% base::as.data.frame()
tot_word_freq %>% arrange(desc(n)) %>% group_by(hashtag) %>% 
  top_n(15) %>% ungroup %>%
  ggplot(aes(word, n, fill = hashtag)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Count") +
  facet_wrap(~hashtag, ncol = 2, scales = "free") +
  coord_flip() + labs(y = "Frequency of Use", x = "Word",
  title = "Most Used Words by Hashtag")
# Plot of retweets vs non-retweets in bar graph form
ggplot(totaltweet) + geom_bar(aes(x = isRetweet, fill = isRetweet),
  stat = 'count') + geom_text(stat = 'count', 
  aes(x = isRetweet,label=..count..), vjust = 3) + 
  labs(x = "Retweeted Tweets", y = "Count", 
  fill = "Retweeted", 
  title = "Retweeted and Non-Retweeted\nTweets for All Hashtags")
# Retweet plot by hashtag
ggplot(totaltweet) + geom_bar(aes(x = isRetweet, fill = hashtag)) + 
  facet_wrap(~hashtag) + geom_text(stat = 'count',
  aes(x = isRetweet, label=..count..), vjust = 1) +
  labs(x = "Retweeted Tweets", y = "Count", fill = "Hashtag",
  title = "Retweeted and Non-Retweeted\nTweets per Hashtag")
# Create map template and mapped the location of tweets.
worldMap <- map_data("world")
loc_plot <- ggplot(worldMap)
loc_plot + geom_path(aes(x = long, y = lat, group = group)) +
  geom_point(data = location,aes(x=lon,y=lat),color="RED",
  size=.25) + labs(x = "Longitude", y = "Latitude", 
  title = "Map of Location for #mentalhealthawarness Tweets")
# Wordcloud with traditional wordcloud package of all tweets together
# Cleaning was done using dplyr package for this data.
totaltweet %>%
  ungroup() %>% filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", 
  pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word, 
  str_detect(word, "[a-z]")) %>%
  dplyr::count(word) %>%
  with(wordcloud(word, n, max.words = 100,
  colors=brewer.pal(8,"Dark2"), 
  scale=c(1.5,0.3), random.order=F))
# Creating a dataframe with frequencies of words for each hashtag
hashtag_words <- totaltweet %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", 
  pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
  str_detect(word, "[a-z]")) %>% 
  dplyr::count(hashtag, word, sort = TRUE) %>% ungroup()
# Group by hashtag in order to calcuate frequency per hashtag
total_words <- hashtag_words %>% group_by(hashtag) %>%
  dplyr::summarize(total = sum(n))
# Join the dataframes with left outer join
hashtag_words <- left_join(hashtag_words,total_words)
# Plot of word frequency per hashtag in histogram form.
ggplot(hashtag_words, aes(n/total, fill = hashtag)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~hashtag, ncol = 2, scales = "free_y") +
  labs(x = "Percentage of Total Word Usage", y = "Frequency", 
  title = "Word Frequency Distribution per Hashtag")
# Add in tf-idf values to the dataset.
hashtag_tfidf <- hashtag_words %>% bind_tf_idf(word, hashtag, n)
# Arrange by tf-idf score
hashtag_tfidf <- hashtag_tfidf %>% dplyr::select(-total) %>%
  arrange(desc(tf_idf))
# Plot of word frequencies based on tf-idf scores with top 15 words
# for each of the hashtags
hashtag_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(hashtag) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = hashtag)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~hashtag, ncol = 2, scales = "free") +
  coord_flip() + labs(y = "Frequency of Use", x = "Word",
  title = "Most Used Words by Hashtag")
# Create dataframe of word frequencies per hashtag for Zipf's plots
freq_by_rank <- hashtag_words %>% 
  group_by(hashtag) %>% 
  mutate(rank = row_number(), `term frequency` = n/total)
# Plot of word frequencies based on tf-idf scores as Zipf's plots
# For each of the hashtags
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = hashtag)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) + 
  scale_x_log10() + scale_y_log10() + labs(x = "Rank", 
  y = "Term Frequency", 
  title = "Zipf's Plot of each Hashtag", color = "Hashtag")
# Comparison plot between K-medoids clustering, LDA topic modeling
# and LSA topic modeling.
grid.arrange(cluster_plot,lda_plot,lsa_plot, nrow = 2, ncol = 2)