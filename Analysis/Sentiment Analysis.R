# Sentiment analysis code adapted from:
# https://www.tidytextmining.com/

# Joined bing sentiments to tidy datframe of tweets
tweet_sentiment <- tidytweet %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(hashtag, index = linenumber %/% 50,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Joined nrc sentiments to tidy dataframe of tweets
tweet_sentiment_nrc <- tidytweet %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(index = linenumber %/% 50, hashtag, sentiment) %>%
  summarize(n = n())

# Plot of sentiment per segment values for each setiment group colored
# by with hashtag the segments appeared in
ggplot(tweet_sentiment_nrc) + 
  geom_bar(aes(x = index, y = n, fill = hashtag), 
           stat = "identity") + facet_wrap(~sentiment, ncol = 5)

# Plot of negative and positive sentiment values for tweet segments in
# each of the hashtags in order to compare sentiment values throughout
# the tweets of each hashtag
ggplot(tweet_sentiment,aes(index, sentiment,fill = hashtag)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~hashtag, ncol = 2, scales = "free_x")

# Create a dataframe with counts for each bing sentiment
bing_word_counts <- tidytweet %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Create a dataframe of bing sentiment counts grouped by hashtag
bing_word_hashtag_counts <- tidytweet %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, hashtag, sentiment, sort = TRUE) %>%
  ungroup()

# Plot of top 15 positive and negative words
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",x = NULL) +
  coord_flip()

# Plot of top 5 positive and negative words for each hashtag
bing_word_hashtag_counts %>%
  group_by(hashtag,sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~hashtag, scales = "free_y") +
  labs(y = "Contribution to sentiment",x = NULL) +
  coord_flip()

# Dataframe with nrc sentiment counts
bing_word_counts_nrc <- tidytweet %>%
  inner_join(get_sentiments("nrc")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Plot of top 10 words for each nrc sentiment grouping
bing_word_counts_nrc %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",x = NULL) +
  coord_flip()

# Dataframe of nrc sentient counts by hashtag
bing_word_hashtag_counts_nrc <- tidytweet %>%
  inner_join(get_sentiments("nrc")) %>%
  dplyr::count(word, hashtag, sentiment, sort = TRUE) %>%
  ungroup()

# Plot of top two words in each nrc sentiment for each hashtag
bing_word_hashtag_counts_nrc %>%
  group_by(hashtag,sentiment) %>%
  top_n(2) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = hashtag)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",x = NULL) +
  coord_flip()

# Comparison cloud with positive and negative bing sentiment words
tidytweet %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100,scale=c(3,.15))