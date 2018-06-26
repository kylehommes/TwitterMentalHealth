# Libraries needed for analysis
library(tm)
library(dplyr)
library(stringr)
library(NbClust)
library(lsa)
library(cluster)
library(factoextra)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

# Clustering with All Hashtags
# Set seed
set.seed(1234)
# Remove sparse terms and weeight as tf-idf
tottdm_sparse <- removeSparseTerms(tottdm, sparse = 0.999)
tottdm_tfidf <- weightTfIdf(tottdm_sparse)
# Create LSA matrices to improve clustering
totlsa_topics_clust <- lsa(tottdm_tfidf, dims = 10)
# Use elbow plot to determine the optimal number of clusters
fviz_nbclust(totlsa_topics_clust$tk, pam, k.max = 50, method = "wss") +
  theme_classic() + geom_vline(xintercept = 12, linetype = 2)
# Create data frame using LSA terms matrix
totlsa.tk <- as.data.frame(totlsa_topics_clust$tk)
# Cluster with K-Medoids(PAM algorithm) using manhattan distance
totclust <- pam(totlsa.tk, 12, metric = "manhattan")
# Inspect clusters
head(totclust$clustering, 10)
# Create dataframe with clusters and words in it
clusters <- as.data.frame(totclust$clustering)
clusters$word <- row.names(clusters)
colnames(clusters) <- c("cluster", "word")
hashtag_word_clust <- hashtag_words %>% inner_join(clusters)
# Plot the clusters and words
tot_clust_words_plot <- hashtag_word_clust %>%
  group_by(cluster) %>% 
  top_n(10, n) %>% 
  ungroup %>%
  ggplot(aes(word, n, fill = factor(cluster))) +
  geom_col(show.legend = TRUE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~cluster, ncol = 3, scales = "free") +
  coord_flip() + labs(y = "Frequency of Use", x = "Word",
  title = "Top 5 words per cluster", fill = "Cluster")
# Plot number of words per cluster
tot_clust_words_plot
cluster_plot <- ggplot(hashtag_word_clust) + 
  geom_bar(aes(x = factor(cluster), 
  fill = factor(cluster)), show.legend = FALSE) +
  geom_text(aes(x = factor(cluster), label = ..count..),
  stat = "count", vjust = -1) + labs(x = "Cluster", 
  y = "Number of Words", title = "Number of Words per Cluster") +
  facet_wrap(~hashtag, scales = "free") + ylim(0, 1000)
cluster_plot

# Repeat steps from above with each of the hashtags
mhatdm_sparse <- removeSparseTerms(mhatdm, sparse = 0.999)
mhatdm_tfidf <- weightTfIdf(mhatdm_sparse)
mhalsa_topics_clust <- lsa(mhatdm_tfidf, dims = 10)
fviz_nbclust(mhalsa_topics_clust$tk, pam, k.max = 50, method = "wss") +
  theme_classic() + geom_vline(xintercept = 12, linetype = 2)
mhalsa.tk <- as.data.frame(mhalsa_topics_clust$tk)
mhaclust <- pam(mhalsa.tk, 12, metric = "manhattan")
head(mhaclust$clustering, 10)
mha_clusters <- as.data.frame(mhaclust$clustering)
mha_clusters$word <- row.names(mha_clusters)
colnames(mha_clusters) <- c("cluster", "word")
mha_hashtag_word_clust <- hashtag_words %>% filter(hashtag == "MHA") %>%
  inner_join(mha_clusters)
mha_clust_words_plot <- mha_hashtag_word_clust %>%
  group_by(cluster) %>% 
  top_n(10, n) %>% 
  ungroup %>%
  ggplot(aes(word, n, fill = factor(cluster))) +
  geom_col(show.legend = TRUE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~cluster, ncol = 3, scales = "free") +
  coord_flip() + labs(y = "Frequency of Use", x = "Word",
  title = "Top 5 words per cluster in #mentalhealthawareness", 
  fill = "Cluster")
mha_clust_words_plot
mha_cluster_plot <- ggplot(mha_hashtag_word_clust) + 
  geom_bar(aes(x = factor(cluster), 
  fill = factor(cluster)), show.legend = FALSE) +
  geom_text(aes(x = factor(cluster), label = ..count..),
  stat = "count", vjust = -1) + labs(x = "Cluster", 
  y = "Number of Words", title = "Number of Words per Cluster
  in #mentalhealthawareness") + ylim(0, 2500)
mha_cluster_plot

anxtdm_sparse <- removeSparseTerms(atdm, sparse = 0.999)
anxtdm_tfidf <- weightTfIdf(anxtdm_sparse)
anxlsa_topics_clust <- lsa(anxtdm_tfidf, dims = 10)
fviz_nbclust(anxlsa_topics_clust$tk, pam, k.max = 50, method = "wss") +
  theme_classic() + geom_vline(xintercept = 12, linetype = 2)
anxlsa.tk <- as.data.frame(anxlsa_topics_clust$tk)
anxclust <- pam(anxlsa.tk, 12, metric = "manhattan")
head(anxclust$clustering, 10)
anx_clusters <- as.data.frame(anxclust$clustering)
anx_clusters$word <- row.names(anx_clusters)
colnames(anx_clusters) <- c("cluster", "word")
anx_hashtag_word_clust <- hashtag_words %>% filter(hashtag == "Anxiety") %>% 
  inner_join(anx_clusters)
anx_clust_words_plot <- anx_hashtag_word_clust %>%
  group_by(cluster) %>% 
  top_n(10, n) %>% 
  ungroup %>%
  ggplot(aes(word, n, fill = factor(cluster))) +
  geom_col(show.legend = TRUE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~cluster, ncol = 3, scales = "free") +
  coord_flip() + labs(y = "Frequency of Use", x = "Word",
  title = "Top 5 words per cluster in #anxiety", 
  fill = "Cluster")
anx_clust_words_plot
anx_cluster_plot <- ggplot(anx_hashtag_word_clust) + 
  geom_bar(aes(x = factor(cluster), 
  fill = factor(cluster)), show.legend = FALSE) +
  geom_text(aes(x = factor(cluster), label = ..count..),
  stat = "count", vjust = -1) + labs(x = "Cluster", 
  y = "Number of Words", title = "Number of Words per Cluster
  in #anxiety") + ylim(0, 2500)
anx_cluster_plot

deptdm_sparse <- removeSparseTerms(dtdm, sparse = 0.999)
deptdm_tfidf <- weightTfIdf(deptdm_sparse)
deplsa_topics_clust <- lsa(mhatdm_tfidf, dims = 10)
fviz_nbclust(deplsa_topics_clust$tk, pam, k.max = 50, method = "wss") +
  theme_classic() + geom_vline(xintercept = 12, linetype = 2)
deplsa.tk <- as.data.frame(deplsa_topics_clust$tk)
depclust <- pam(deplsa.tk, 12, metric = "manhattan")
head(depclust$clustering, 10)
dep_clusters <- as.data.frame(depclust$clustering)
dep_clusters$word <- row.names(dep_clusters)
colnames(dep_clusters) <- c("cluster", "word")
dep_hashtag_word_clust <- hashtag_words %>% filter(hashtag == "Depression") %>%
  inner_join(dep_clusters)
dep_clust_words_plot <- dep_hashtag_word_clust %>%
  group_by(cluster) %>% 
  top_n(10, n) %>% 
  ungroup %>%
  ggplot(aes(word, n, fill = factor(cluster))) +
  geom_col(show.legend = TRUE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~cluster, ncol = 3, scales = "free") +
  coord_flip() + labs(y = "Frequency of Use", x = "Word",
  title = "Top 5 words per cluster in #depression", fill = "Cluster")
dep_clust_words_plot
dep_cluster_plot <- ggplot(dep_hashtag_word_clust) + 
  geom_bar(aes(x = factor(cluster), 
  fill = factor(cluster)), show.legend = FALSE) +
  geom_text(aes(x = factor(cluster), label = ..count..),
  stat = "count", vjust = -1) + labs(x = "Cluster", 
  y = "Number of Words", title = "Number of Words per Cluster
  in #depression") + ylim(0, 2500)
dep_cluster_plot

ptsdtdm_sparse <- removeSparseTerms(ptdm, sparse = 0.999)
ptsdtdm_tfidf <- weightTfIdf(ptsdtdm_sparse)
ptsdlsa_topics_clust <- lsa(ptsdtdm_tfidf, dims = 10)
fviz_nbclust(ptsdlsa_topics_clust$tk, pam, k.max = 50, method = "wss") +
  theme_classic() + geom_vline(xintercept = 14, linetype = 2)
ptsdlsa.tk <- as.data.frame(ptsdlsa_topics_clust$tk)
ptsdclust <- pam(ptsdlsa.tk, 14, metric = "manhattan")
head(ptsdclust$clustering, 10)
ptsd_clusters <- as.data.frame(ptsdclust$clustering)
ptsd_clusters$word <- row.names(ptsd_clusters)
colnames(ptsd_clusters) <- c("cluster", "word")
ptsd_hashtag_word_clust <- hashtag_words %>% filter(hashtag == "PTSD") %>% 
  inner_join(ptsd_clusters)
ptsd_clust_words_plot <- ptsd_hashtag_word_clust %>%
  group_by(cluster) %>% 
  top_n(10, n) %>% 
  ungroup %>%
  ggplot(aes(word, n, fill = factor(cluster))) +
  geom_col(show.legend = TRUE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~cluster, ncol = 3, scales = "free") +
  coord_flip() + labs(y = "Frequency of Use", x = "Word",
  title = "Top 5 words per cluster in #ptsd", 
  fill = "Cluster")
ptsd_clust_words_plot
ptsd_cluster_plot <- ggplot(ptsd_hashtag_word_clust) + 
  geom_bar(aes(x = factor(cluster), 
  fill = factor(cluster)), show.legend = FALSE) +
  geom_text(aes(x = factor(cluster), label = ..count..),
  stat = "count", vjust = -1) + labs(x = "Cluster", 
  y = "Number of Words", title = "Number of Words per Cluster
  in #ptsd") + ylim(0, 2500)
ptsd_cluster_plot

suitdm_sparse <- removeSparseTerms(stdm, sparse = 0.999)
suitdm_tfidf <- weightTfIdf(suitdm_sparse)
suilsa_topics_clust <- lsa(suitdm_tfidf, dims = 10)
fviz_nbclust(suilsa_topics_clust$tk, pam, k.max = 50, method = "wss") +
  theme_classic() + geom_vline(xintercept = 12, linetype = 2)
suilsa.tk <- as.data.frame(anxlsa_topics_clust$tk)
suiclust <- pam(suilsa.tk, 12, metric = "manhattan")
head(suiclust$clustering, 10)
sui_clusters <- as.data.frame(suiclust$clustering)
sui_clusters$word <- row.names(sui_clusters)
colnames(sui_clusters) <- c("cluster", "word")
sui_hashtag_word_clust <- hashtag_words %>% filter(hashtag == "Suicide") %>%
  inner_join(sui_clusters)
sui_clust_words_plot <- sui_hashtag_word_clust %>%
  group_by(cluster) %>% 
  top_n(10, n) %>% 
  ungroup %>%
  ggplot(aes(word, n, fill = factor(cluster))) +
  geom_col(show.legend = TRUE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~cluster, ncol = 3, scales = "free") +
  coord_flip() + labs(y = "Frequency of Use", x = "Word",
  title = "Top 5 words per cluster in #suicide", 
  fill = "Cluster")
sui_clust_words_plot
sui_cluster_plot <- ggplot(sui_hashtag_word_clust) + 
  geom_bar(aes(x = factor(cluster), 
  fill = factor(cluster)), show.legend = FALSE) +
  geom_text(aes(x = factor(cluster), label = ..count..),
  stat = "count", vjust = -1) + labs(x = "Cluster", 
  y = "Number of Words", title = "Number of Words per Cluster
  in #suicide") + ylim(0, 2500)
sui_cluster_plot