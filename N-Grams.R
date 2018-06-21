# Code for n-grams was adapted from:
# https://www.tidytextmining.com/

#Libraries needed
library(tidyr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

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
bigrams_united_freq <- bigrams_united %>% dplyr::count(bigram, 
  sort = TRUE)
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
bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(hashtag) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  dplyr::mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = hashtag)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ hashtag, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of bigram to hashtag", x = "")
# Find the tf-idf values for trigrams
trigram_tf_idf <- trigrams_united %>% 
  dplyr::count(hashtag, trigram) %>%
  bind_tf_idf(trigram, hashtag, n) %>%
  arrange(desc(tf_idf))
# Plot the top 12 trigrams for each hashtag
trigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(hashtag) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  dplyr::mutate(trigram = reorder(trigram, tf_idf)) %>%
  ggplot(aes(trigram, tf_idf, fill = hashtag)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ hashtag, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of trigram to hashtag", x = "")
# Find the tf-idf values for 4-grams
fourgram_tf_idf <- fourgrams_united %>% 
  dplyr::count(hashtag, fourgram) %>%
  bind_tf_idf(fourgram, hashtag, n) %>%
  arrange(desc(tf_idf))
# Plot the top 12 4-grams for each hashtag
fourgram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(hashtag) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  dplyr::mutate(fourgram = reorder(fourgram, tf_idf)) %>%
  ggplot(aes(fourgram, tf_idf, fill = hashtag)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ hashtag, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of 4-gram to hashtag", x = "")