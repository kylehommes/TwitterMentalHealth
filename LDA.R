# Libraries needed
library(topicmodels)
library(tm)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(reshape2)

# Filtered dataset to removing empty rows
ui = unique(totdtm$i)
totdtm_new = totdtm[ui,]
# Created an LDA model with 10 topics
tweet_LDA <- LDA(totdtm_new, k=10, control = list(seed = 1234))
# Made the LDA model into a tidy dataframe for easier preparation
tweet_topics <- tidy(tweet_LDA, matrix = "beta")
# Group by topic
tweet_top_terms <- tweet_topics %>% group_by(topic) %>% 
  top_n(15, beta) %>% ungroup() %>% arrange(topic, -beta)
# Mutate and plot top 15 words per topic
tot_lda_words_plot <- tweet_top_terms %>% 
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + labs(x = "Words", 
  y = "Frequency of Use in Each Topic", 
  title = "Top 15 Words in Each Topic")
tot_lda_words_plot
# Create dataset to see how many words are in each topic
lda_topics <- tweet_topics %>% group_by(term) %>% summarize(beta = max(beta))
lda_tweet_topics <- lda_topics %>% inner_join(tweet_topics)
colnames(lda_tweet_topics)[1] <- "word"
combo <- tidytweet[,c(16,18)]
lda_combo <- combo %>% inner_join(lda_tweet_topics)
# Plot of words per topic for each hashtag
lda_plot <- ggplot(lda_combo) + 
  geom_bar(aes(x = factor(topic), fill = factor(topic)), 
  show.legend = FALSE) + 
  geom_text(aes( x = factor(topic), label = ..count..),
  stat = "count", vjust = -1) + ylim(-100,10000) +
  labs(x = "Topic", y = "Count", 
  title = "Number of Words most associated 
  with Each LDA Topic by hashtag") +
  facet_wrap(~hashtag, scales = "free")
lda_plot

# Filtered dataset to removing empty rows
ui = unique(mhadtm$i)
mhadtm_new = mhadtm[ui,]
# Created an LDA model with 10 topics
mha_tweet_LDA <- LDA(mhadtm_new, k=10, control = list(seed = 1234))
# Made the LDA model into a tidy dataframe for easier preparation
mha_tweet_topics <- tidy(mha_tweet_LDA, matrix = "beta")
# Group by topic
mha_tweet_top_terms <- mha_tweet_topics %>% group_by(topic) %>% 
  top_n(15, beta) %>% ungroup() %>% arrange(topic, -beta)
# Mutate and plot top 15 words per topic
mha_lda_words_plot <- mha_tweet_top_terms %>% 
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + labs(x = "Words", 
  y = "Frequency of Use in Each Topic", 
  title = "Top 15 Words in Each Topic for #mentalhealthawareness")
mha_lda_words_plot
# Create dataset to see how many words are in each topic
mha_lda_topics <- mha_tweet_topics %>% group_by(term) %>% 
  summarize(beta = max(beta))
mha_lda_tweet_topics <- mha_lda_topics %>% inner_join(mha_tweet_topics)
colnames(mha_lda_tweet_topics)[1] <- "word"
mha_combo <- tidytweet[,c(16,18)] %>% filter(hashtag == "MHA")
mha_lda_combo <- mha_combo %>% inner_join(mha_lda_tweet_topics)
# Plot of words per topic for each hashtag
mha_lda_plot <- ggplot(mha_lda_combo) + 
  geom_bar(aes(x = factor(topic), fill = factor(topic)), 
  show.legend = FALSE) + 
  geom_text(aes( x = factor(topic), label = ..count..),
  stat = "count", vjust = -1) + ylim(-100,15000) +
  labs(x = "Topic", y = "Count", 
  title = "Number of Words most associated 
  with Each LDA Topic in #mentalhealthawareness")
mha_lda_plot

# Filtered dataset to removing empty rows
ui = unique(sdtm$i)
suidtm_new = sdtm[ui,]
# Created an LDA model with 10 topics
sui_tweet_LDA <- LDA(suidtm_new, k=10, control = list(seed = 1234))
# Made the LDA model into a tidy dataframe for easier preparation
sui_tweet_topics <- tidy(sui_tweet_LDA, matrix = "beta")
# Group by topic
sui_tweet_top_terms <- sui_tweet_topics %>% group_by(topic) %>% 
  top_n(15, beta) %>% ungroup() %>% arrange(topic, -beta)
# Mutate and plot top 15 words per topic
sui_lda_words_plot <- sui_tweet_top_terms %>% 
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + labs(x = "Words", 
  y = "Frequency of Use in Each Topic", 
  title = "Top 15 Words in Each Topic for #suicide")
sui_lda_words_plot
# Create dataset to see how many words are in each topic
sui_lda_topics <- sui_tweet_topics %>% group_by(term) %>% 
  summarize(beta = max(beta))
sui_lda_tweet_topics <- sui_lda_topics %>% inner_join(sui_tweet_topics)
colnames(sui_lda_tweet_topics)[1] <- "word"
sui_combo <- tidytweet[,c(16,18)]
sui_lda_combo <- sui_combo %>% inner_join(sui_lda_tweet_topics) %>%
  filter(hashtag == "Suicide")
# Plot of words per topic for each hashtag
sui_lda_plot <- ggplot(sui_lda_combo) + 
  geom_bar(aes(x = factor(topic), fill = factor(topic)), 
  show.legend = FALSE) + 
  geom_text(aes( x = factor(topic), label = ..count..),
  stat = "count", vjust = -1) + ylim(-100,15000) +
  labs(x = "Topic", y = "Count", 
  title = "Number of Words most associated 
  with Each LDA Topic in #suicide")
sui_lda_plot

# Filtered dataset to removing empty rows
ui = unique(ddtm$i)
depdtm_new = ddtm[ui,]
# Created an LDA model with 10 topics
dep_tweet_LDA <- LDA(depdtm_new, k=10, control = list(seed = 1234))
# Made the LDA model into a tidy dataframe for easier preparation
dep_tweet_topics <- tidy(dep_tweet_LDA, matrix = "beta")
# Group by topic
dep_tweet_top_terms <- dep_tweet_topics %>% group_by(topic) %>% 
  top_n(15, beta) %>% ungroup() %>% arrange(topic, -beta)
# Mutate and plot top 15 words per topic
dep_lda_words_plot <- dep_tweet_top_terms %>% 
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + labs(x = "Words", 
  y = "Frequency of Use in Each Topic", 
  title = "Top 15 Words in Each Topic for #depression")
dep_lda_words_plot
# Create dataset to see how many words are in each topic
dep_lda_topics <- dep_tweet_topics %>% group_by(term) %>% 
  summarize(beta = max(beta))
dep_lda_tweet_topics <- dep_lda_topics %>% inner_join(dep_tweet_topics)
colnames(dep_lda_tweet_topics)[1] <- "word"
dep_combo <- tidytweet[,c(16,18)]
dep_lda_combo <- dep_combo %>% inner_join(dep_lda_tweet_topics) %>%
  filter(hashtag == "Depression")
# Plot of words per topic for each hashtag
dep_lda_plot <- ggplot(dep_lda_combo) + 
  geom_bar(aes(x = factor(topic), fill = factor(topic)), 
  show.legend = FALSE) + 
  geom_text(aes( x = factor(topic), label = ..count..),
  stat = "count", vjust = -1) + ylim(-100,15000) +
  labs(x = "Topic", y = "Count", 
  title = "Number of Words most associated 
  with Each LDA Topic in #depression")
dep_lda_plot

# Filtered dataset to removing empty rows
ui = unique(adtm$i)
anxdtm_new = adtm[ui,]
# Created an LDA model with 10 topics
anx_tweet_LDA <- LDA(anxdtm_new, k=10, control = list(seed = 1234))
# Made the LDA model into a tidy dataframe for easier preparation
anx_tweet_topics <- tidy(anx_tweet_LDA, matrix = "beta")
# Group by topic
anx_tweet_top_terms <- anx_tweet_topics %>% group_by(topic) %>% 
  top_n(15, beta) %>% ungroup() %>% arrange(topic, -beta)
# Mutate and plot top 15 words per topic
anx_lda_words_plot <- anx_tweet_top_terms %>% 
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + labs(x = "Words", 
  y = "Frequency of Use in Each Topic", 
  title = "Top 15 Words in Each Topic for #anxiety")
anx_lda_words_plot
# Create dataset to see how many words are in each topic
anx_lda_topics <- anx_tweet_topics %>% group_by(term) %>% 
  summarize(beta = max(beta))
anx_lda_tweet_topics <- anx_lda_topics %>% inner_join(anx_tweet_topics)
colnames(anx_lda_tweet_topics)[1] <- "word"
anx_combo <- tidytweet[,c(16,18)]
anx_lda_combo <- anx_combo %>% inner_join(anx_lda_tweet_topics) %>%
  filter(hashtag == "Anxiety")
# Plot of words per topic for each hashtag
anx_lda_plot <- ggplot(anx_lda_combo) + 
  geom_bar(aes(x = factor(topic), fill = factor(topic)), 
  show.legend = FALSE) + 
  geom_text(aes( x = factor(topic), label = ..count..),
  stat = "count", vjust = -1) + ylim(-100,15000) +
  labs(x = "Topic", y = "Count", 
  title = "Number of Words most associated 
  with Each LDA Topic in #anxiety")
anx_lda_plot

# Filtered dataset to removing empty rows
ui = unique(pdtm$i)
ptsddtm_new = pdtm[ui,]
# Created an LDA model with 10 topics
ptsd_tweet_LDA <- LDA(ptsddtm_new, k=10, control = list(seed = 1234))
# Made the LDA model into a tidy dataframe for easier preparation
ptsd_tweet_topics <- tidy(ptsd_tweet_LDA, matrix = "beta")
# Group by topic
ptsd_tweet_top_terms <- ptsd_tweet_topics %>% group_by(topic) %>% 
  top_n(15, beta) %>% ungroup() %>% arrange(topic, -beta)
# Mutate and plot top 15 words per topic
ptsd_lda_words_plot <- ptsd_tweet_top_terms %>% 
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + labs(x = "Words", 
  y = "Frequency of Use in Each Topic", 
  title = "Top 15 Words in Each Topic for #ptsd")
ptsd_lda_words_plot
# Create dataset to see how many words are in each topic
ptsd_lda_topics <- ptsd_tweet_topics %>% group_by(term) %>% 
  summarize(beta = max(beta))
ptsd_lda_tweet_topics <- ptsd_lda_topics %>% inner_join(ptsd_tweet_topics)
colnames(ptsd_lda_tweet_topics)[1] <- "word"
ptsd_combo <- tidytweet[,c(16,18)]
ptsd_lda_combo <- ptsd_combo %>% inner_join(ptsd_lda_tweet_topics) %>%
  filter(hashtag == "PTSD")
# Plot of words per topic for each hashtag
ptsd_lda_plot <- ggplot(ptsd_lda_combo) + 
  geom_bar(aes(x = factor(topic), fill = factor(topic)), 
  show.legend = FALSE) + 
  geom_text(aes( x = factor(topic), label = ..count..),
  stat = "count", vjust = -1) + ylim(-100,15000) +
  labs(x = "Topic", y = "Count", 
  title = "Number of Words most associated 
  with Each LDA Topic in #ptsd")
ptsd_lda_plot