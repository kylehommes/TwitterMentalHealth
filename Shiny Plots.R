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
nrc_seg_plot <- ggplot(tweet_sentiment_nrc) + 
  geom_bar(aes(x = index, y = n, fill = hashtag), 
           stat = "identity") + facet_wrap(~sentiment, ncol = 5)
# Plot of negative and positive sentiment values for tweet segments in
# each of the hashtags in order to compare sentiment values throughout
# the tweets of each hashtag
bing_seg_plot <- ggplot(tweet_sentiment,aes(index, sentiment,fill = hashtag)) +
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
bing_plot <- bing_word_counts %>%
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
bing_hashtag_plot <- bing_word_hashtag_counts %>%
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
nrc_plot <- bing_word_counts_nrc %>%
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
nrc_hashtag_plot <- bing_word_hashtag_counts_nrc %>%
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
# Code for n-grams was adapted from:
# https://www.tidytextmining.com/
# Clean the data in order to tokenize in N-Grams
totaltweet$text <- iconv(totaltweet$text, "latin1", "ASCII", 
                         sub = "")
totaltweet$text <- gsub("http(s?)([^ ]*)", " ",
                        totaltweet$text, ignore.case = T)
totaltweet$text <- gsub("&amp", "and", totaltweet$text)
# Create bigrams
tweet_bigrams <- totaltweet %>% unnest_tokens(bigram, text,
                                              token = "ngrams", n = 2)
# Sperate bigrams by word
bigrams_separated <- tweet_bigrams %>% separate(bigram,
                                                c("word1","word2"), sep = " ")
# Filter stopwords out of each word in the bigram
bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
# Create counts for each of the bigrams
bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE)
# Unite the separated words
bigrams_united <- bigrams_filtered %>% unite(bigram, word1,
                                             word2, sep = " ")
# Find frequencies for the bigrams
bigrams_united_freq <- bigrams_united %>% dplyr::count(bigram, sort = TRUE)
# Create trigrams
tweet_trigrams <- totaltweet %>% unnest_tokens(trigram, text,
                                               token = "ngrams", n = 3)
# Separate the trigrams by word
trigrams_separated <- tweet_trigrams %>% separate(trigram,
                                                  c("word1","word2","word3"), sep = " ")
# Filter stopwords out for each word
trigrams_filtered <- trigrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)
# Get couts of trigrams
trigram_counts <- trigrams_filtered %>% 
  dplyr::count(word1, word2, word3, sort = TRUE)
# Unite the trigrams
trigrams_united <- trigrams_filtered %>% unite(trigram, word1,
                                               word2, word3, sep = " ")
# Find the frequency for the trigrams
trigrams_united_freq <- trigrams_united %>% dplyr::count(trigram,
                                                         sort = TRUE)
# Create 4-grams
tweet_4grams <- totaltweet %>% unnest_tokens(fourgram, text,
                                             token = "ngrams", n = 4)
# Separate the 4-grams by words
fourgrams_separated <- tweet_4grams %>% separate(fourgram,
                                                 c("word1","word2","word3","word4"), sep = " ")
# Filter out the stopwords from each word
fourgrams_filtered <- fourgrams_separated %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word)
# Count the 4-grams
fourgram_counts <- fourgrams_filtered %>% 
  dplyr::count(word1, word2, word3, word4, sort = TRUE)
# Unite the 4-grams
fourgrams_united <- fourgrams_filtered %>% unite(fourgram, word1,
                                                 word2, word3, word4, sep = " ")
# Find the frequency for the 4-grams
fourgrams_united_freq <- fourgrams_united %>%
  dplyr::count(fourgram, sort = TRUE)
# Find tf-idf values for bigrams
bigram_tf_idf <- bigrams_united %>% 
  dplyr::count(hashtag, bigram) %>%
  bind_tf_idf(bigram, hashtag, n) %>%
  arrange(desc(tf_idf))
# Plot top 12 bigrams for each hashtag
bigram_plot <- bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(hashtag) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  dplyr::mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = hashtag)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ hashtag, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of bigram to hashtag",
       x = "")
# Find the tf-idf values for trigrams
trigram_tf_idf <- trigrams_united %>% 
  dplyr::count(hashtag, trigram) %>%
  bind_tf_idf(trigram, hashtag, n) %>%
  arrange(desc(tf_idf))
# Plot the top 12 trigrams for each hashtag
trigram_plot <- trigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(hashtag) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  dplyr::mutate(trigram = reorder(trigram, tf_idf)) %>%
  ggplot(aes(trigram, tf_idf, fill = hashtag)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ hashtag, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of trigram to hashtag",
       x = "")
# Find the tf-idf values for 4-grams
fourgram_tf_idf <- fourgrams_united %>% 
  dplyr::count(hashtag, fourgram) %>%
  bind_tf_idf(fourgram, hashtag, n) %>%
  arrange(desc(tf_idf))
# Plot the top 12 4-grams for each hashtag
fourgram_plot <- fourgram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(hashtag) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  dplyr::mutate(fourgram = reorder(fourgram, tf_idf)) %>%
  ggplot(aes(fourgram, tf_idf, fill = hashtag)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ hashtag, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of 4-gram to hashtag",
       x = "")

