# Set seed
set.seed(1234)

# Function to create top 5 words per LSA topic
lsa_func <- function(x) {
  term_topics <- as.data.frame(x)
  colnames(term_topics) <- c("TopicA", "TopicB", "TopicC", "TopicD",
  "TopicE", "TopicF", "TopicG", "TopicH", "TopicI", "TopicJ")
  term_top_list <- 
    as.data.frame(colnames(term_topics)[apply(term_topics,
    1,which.max)])
  colnames(term_top_list) <- "Topic"
  term_topics$word <- row.names(term_topics)
  term_topics <- term_topics %>% bind_cols(term_top_list)
  term_topics1 <- term_topics %>% dplyr::select(TopicA,word) %>%
    arrange(desc(TopicA)) %>% mutate(label = "TopicA",
    freq = TopicA) %>% as.data.frame()
  term_topics1 <- term_topics1[c(1:5),]
  term_topics2 <- term_topics %>% dplyr::select(TopicB,word) %>%
    arrange(desc(TopicB)) %>% mutate(label = "TopicB",
    freq = TopicB) %>% as.data.frame()
  term_topics2 <- term_topics2[c(1:5),]
  term_topics3 <- term_topics %>% dplyr::select(TopicC,word) %>%
    arrange(desc(TopicC)) %>% mutate(label = "TopicC",
    freq = TopicC) %>% as.data.frame()
  term_topics3 <- term_topics3[c(1:5),]
  term_topics4 <- term_topics %>% dplyr::select(TopicD,word) %>%
    arrange(desc(TopicD)) %>% mutate(label = "TopicD",
    freq = TopicD) %>% as.data.frame()
  term_topics4 <- term_topics4[c(1:5),]
  term_topics5 <- term_topics %>% dplyr::select(TopicE,word) %>%
    arrange(desc(TopicE)) %>% mutate(label = "TopicE",
    freq = TopicE) %>% as.data.frame()
  term_topics5 <- term_topics5[c(1:5),]
  term_topics6 <- term_topics %>% dplyr::select(TopicF,word) %>%
    arrange(desc(TopicF)) %>% mutate(label = "TopicF",
    freq = TopicF) %>% as.data.frame()
  term_topics6 <- term_topics6[c(1:5),]
  term_topics7 <- term_topics %>% dplyr::select(TopicG,word) %>%
    arrange(desc(TopicG)) %>% mutate(label = "TopicG",
    freq = TopicG) %>% as.data.frame()
  term_topics7 <- term_topics7[c(1:5),]
  term_topics8 <- term_topics %>% dplyr::select(TopicH,word) %>%
    arrange(desc(TopicH)) %>% mutate(label = "TopicH",
    freq = TopicH) %>% as.data.frame()
  term_topics8 <- term_topics8[c(1:5),]
  term_topics9 <- term_topics %>% dplyr::select(TopicI,word) %>%
    arrange(desc(TopicI)) %>% mutate(label = "TopicI",
    freq = TopicI) %>% as.data.frame()
  term_topics9 <- term_topics9[c(1:5),]
  term_topics10 <- term_topics %>% dplyr::select(TopicJ,word) %>%
    arrange(desc(TopicJ)) %>% mutate(label = "TopicJ",
    freq = TopicJ) %>% as.data.frame()
  term_topics10 <- term_topics10[c(1:5),]
  term_topics_tot <- bind_rows(term_topics1,term_topics2,term_topics3,
    term_topics4,term_topics5,term_topics6,term_topics7,term_topics8,
    term_topics9,term_topics10)
}

# Remove sparse terms from the tdm
tottdm_sparse_lsa <- removeSparseTerms(tottdm, sparse = 0.99)
# Use tf-idf weighting on matrix
tottdm_tfidf_lsa <- weightTfIdf(tottdm_sparse_lsa)
# Create the LSA matrices
totlsa_topics <- lsa(tottdm_tfidf_lsa, dims = 10)
# Use the LSA terms matrix to find the most similar term
# for each LSA topic
which.max(totlsa_topics$tk[,1])
which.max(totlsa_topics$tk[,2])
which.max(totlsa_topics$tk[,3])
which.max(totlsa_topics$tk[,4])
which.max(totlsa_topics$tk[,5])
which.max(totlsa_topics$tk[,6])
which.max(totlsa_topics$tk[,7])
which.max(totlsa_topics$tk[,8])
which.max(totlsa_topics$tk[,9])
which.max(totlsa_topics$tk[,10])
# Create a dataframe from the documents/tweets lsa matrix
docu <- as.data.frame(totlsa_topics$dk)
# Change column names
colnames(docu) <- c("TopicA", "TopicB", "TopicC", "TopicD",
  "TopicE", "TopicF", "TopicG", "TopicH", "TopicI", "TopicJ")
# Find the most relevant topic for each tweet
doc_topics <- as.data.frame(colnames(docu)[apply(docu,1,which.max)])
# Rename column containing most relevant topic
colnames(doc_topics) <- "Topic"
# Add in count of zero for Topic E for graph
# No tweets showed Topic E as the most relevant Topic to the tweet
df.add <- data.frame(Topic = "TopicE", n = 0)
# Count tweets for each relevant topic and add Topic E row
doc_topics_count <- doc_topics %>% dplyr::count(Topic) %>%
  as.data.frame() %>% bind_rows(df.add)
# Plot the number of tweets for each topic
ggplot(doc_topics_count) + 
  geom_bar(aes(x = factor(Topic), y = n, fill = factor(Topic)), 
  stat = "identity", show.legend = FALSE) + 
  geom_text(aes( x = factor(Topic), y = n, label = n),
  stat = "identity", vjust = -1) + ylim(-1000,40000) +
  labs(x = "Topic", y = "Count", 
  title = "Number of Tweets most associated with Each LSA Topic")
hashtag_tweet <- data.frame(hashtag = totaltweet$hashtag)
hashtag_tweet$Tweet <- rownames(hashtag_tweet)
doc_topics$Tweet <- rownames(doc_topics)
tweet_combo_lsa <- hashtag_tweet %>% inner_join(doc_topics)
ggplot(tweet_combo_lsa) + 
  geom_bar(aes(x = factor(Topic), fill = factor(Topic)), 
  show.legend = FALSE) + 
  geom_text(aes( x = factor(Topic), label = ..count..),
  stat = "count", vjust = -1) + ylim(-1000,15000) +
  labs(x = "Topic", y = "Count", 
  title = "Number of Tweets most associated 
  with Each LSA Topic by Hashtag") +
  facet_wrap(~hashtag, scales = "free")
tot_terms <- lsa_func(totlsa_topics$tk)
ggplot(tot_terms) + geom_bar(aes(x = word, y = freq, 
  fill = factor(label)), stat = "identity", show.legend = FALSE) +
  facet_wrap(~label,scales = "free") +
  coord_flip() + labs(x = "Words", y = "LSA Similarity Score", 
  title = "Top 5 Words per LSA Topic")
term_topics_2 <- as.data.frame(totlsa_topics$tk)
colnames(term_topics_2) <- c("1", "2", "3", "4", 
  "5", "6", "7", "8", "9", "10")
term_top_list_2 <- 
  as.data.frame(colnames(term_topics_2)[apply(term_topics_2,
  1,which.max)])
colnames(term_top_list_2) <- "Topic"
term_topics_2$word <- row.names(term_topics_2)
term_topics_2 <- term_topics_2 %>% bind_cols(term_top_list_2)
term_combo_lsa <- term_topics %>% inner_join(hashtag_words)
term_combo_lsa_2 <- term_topics_2 %>% inner_join(hashtag_words)
ggplot(term_combo_lsa) + geom_bar(aes(x = factor(Topic), 
  fill = factor(Topic)), show.legend = FALSE) +
  geom_text(aes(x = factor(Topic), label = ..count..),
  stat = "count", vjust = -1) + labs(x = "LSA Topic", 
  y = "Number of Words", 
  title = "Number of Words most associated 
  with each LSA topic per hashtag") +
  facet_wrap(~hashtag, scales = "free") + ylim(0, 25)
lsa_plot <- ggplot(term_combo_lsa_2) + geom_bar(aes(x = factor(Topic), 
  fill = factor(Topic)), show.legend = FALSE) +
  geom_text(aes(x = factor(Topic), label = ..count..),
  stat = "count", vjust = -1) + labs(x = "LSA Topic", 
  y = "Number of Words", 
  title = "Number of Words most associated 
  with each LSA topic by hashtag") +
  facet_wrap(~hashtag, scales = "free") + ylim(0, 30)
lsa_plot

mhatdm_sparse_lsa <- removeSparseTerms(mhatdm, sparse = 0.99)
mhatdm_tfidf_lsa <- weightTfIdf(mhatdm_sparse_lsa)
mhalsa_topics <- lsa(mhatdm_tfidf_lsa, dims = 10)
docu_mha <- as.data.frame(mhalsa_topics$dk)
colnames(docu_mha) <- c("TopicA", "TopicB", "TopicC", "TopicD",
  "TopicE", "TopicF", "TopicG", "TopicH", "TopicI", "TopicJ")
doc_topics_mha <- as.data.frame(colnames(docu_mha)[apply(docu_mha,1,
  which.max)])
colnames(doc_topics_mha) <- "Topic"
df.add_mha <- data.frame(Topic = "TopicB", n = 0)
doc_topics_count_mha <- doc_topics_mha %>% dplyr::count(Topic) %>%
  as.data.frame() %>% bind_rows(df.add_mha)
ggplot(doc_topics_count_mha) + 
  geom_bar(aes(x = factor(Topic), y = n, fill = factor(Topic)), 
  stat = "identity", show.legend = FALSE) + 
  geom_text(aes( x = factor(Topic), y = n, label = n),
  stat = "identity", vjust = -1) + ylim(-100,6000) +
  labs(x = "Topic", y = "Count",
  title = "Number of Tweets most associated with Each 
  LSA Topic for #mentalhealthawarness")
mha_lsa_term <- lsa_func(mhalsa_topics$tk)
ggplot(mha_lsa_term) + geom_bar(aes(x = word, y = freq, 
  fill = factor(label)), stat = "identity", show.legend = FALSE) +
  facet_wrap(~label,scales = "free") +
  coord_flip() + labs(x = "Words", y = "LSA Similarity Score", 
  title = "Top 5 Words per LSA Topic for #mentalhealthawareness")

anxtdm_sparse_lsa <- removeSparseTerms(atdm, sparse = 0.99)
anxtdm_tfidf_lsa <- weightTfIdf(anxtdm_sparse_lsa)
anxlsa_topics <- lsa(anxtdm_tfidf_lsa, dims = 10)
docu_anx <- as.data.frame(anxlsa_topics$dk)
colnames(docu_anx) <- c("TopicA", "TopicB", "TopicC", "TopicD",
  "TopicE", "TopicF", "TopicG", "TopicH", "TopicI", "TopicJ")
doc_topics_anx <- as.data.frame(colnames(docu_anx)[apply(docu_anx,1,
  which.max)])
colnames(doc_topics_anx) <- "Topic"
doc_topics_count_anx <- doc_topics_anx %>% dplyr::count(Topic) %>%
  as.data.frame()
ggplot(doc_topics_count_anx) + 
  geom_bar(aes(x = factor(Topic), y = n, fill = factor(Topic)), 
  stat = "identity", show.legend = FALSE) + 
  geom_text(aes( x = factor(Topic), y = n, label = n),
  stat = "identity", vjust = -1) + ylim(-100,6000) +
  labs(x = "Topic", y = "Count",
  title = "Number of Tweets most associated with Each 
  LSA Topic for #anxiety")
anx_lsa_term <- lsa_func(anxlsa_topics$tk)
ggplot(anx_lsa_term) + geom_bar(aes(x = word, y = freq, 
  fill = factor(label)), stat = "identity", show.legend = FALSE) +
  facet_wrap(~label,scales = "free") +
  coord_flip() + labs(x = "Words", y = "LSA Similarity Score", 
  title = "Top 5 Words per LSA Topic for #anxiety")

ptsdtdm_sparse_lsa <- removeSparseTerms(ptdm, sparse = 0.99)
ptsdtdm_tfidf_lsa <- weightTfIdf(ptsdtdm_sparse_lsa)
ptsdlsa_topics <- lsa(ptsdtdm_tfidf_lsa, dims = 10)
docu_ptsd <- as.data.frame(ptsdlsa_topics$dk)
colnames(docu_ptsd) <- c("TopicA", "TopicB", "TopicC", "TopicD",
  "TopicE", "TopicF", "TopicG", "TopicH", "TopicI", "TopicJ")
doc_topics_ptsd <- as.data.frame(colnames(docu_ptsd)[apply(docu_ptsd,1,
  which.max)])
colnames(doc_topics_ptsd) <- "Topic"
df.add_ptsd <- data.frame(Topic = "TopicB", n = 0)
doc_topics_count_ptsd <- doc_topics_ptsd %>% dplyr::count(Topic) %>%
  as.data.frame() %>% bind_rows(df.add_ptsd)
ggplot(doc_topics_count_ptsd) + 
  geom_bar(aes(x = factor(Topic), y = n, fill = factor(Topic)), 
  stat = "identity", show.legend = FALSE) + 
  geom_text(aes( x = factor(Topic), y = n, label = n),
  stat = "identity", vjust = -1) + ylim(-100,6000) +
  labs(x = "Topic", y = "Count",
  title = "Number of Tweets most associated with Each 
  LSA Topic for #ptsd")
ptsd_lsa_term <- lsa_func(ptsdlsa_topics$tk)
ggplot(ptsd_lsa_term) + geom_bar(aes(x = word, y = freq, 
  fill = factor(label)), stat = "identity", show.legend = FALSE) +
  facet_wrap(~label,scales = "free") +
  coord_flip() + labs(x = "Words", y = "LSA Similarity Score", 
  title = "Top 5 Words per LSA Topic for #ptsd")

deptdm_sparse_lsa <- removeSparseTerms(dtdm, sparse = 0.99)
deptdm_tfidf_lsa <- weightTfIdf(deptdm_sparse_lsa)
deplsa_topics <- lsa(anxtdm_tfidf_lsa, dims = 10)
docu_dep <- as.data.frame(deplsa_topics$dk)
colnames(docu_dep) <- c("TopicA", "TopicB", "TopicC", "TopicD",
  "TopicE", "TopicF", "TopicG", "TopicH", "TopicI", "TopicJ")
doc_topics_dep <- as.data.frame(colnames(docu_dep)[apply(docu_dep,1,
  which.max)])
colnames(doc_topics_dep) <- "Topic"
doc_topics_count_dep <- doc_topics_dep %>% dplyr::count(Topic) %>%
  as.data.frame()
ggplot(doc_topics_count_dep) + 
  geom_bar(aes(x = factor(Topic), y = n, fill = factor(Topic)), 
  stat = "identity", show.legend = FALSE) + 
  geom_text(aes( x = factor(Topic), y = n, label = n),
  stat = "identity", vjust = -1) + ylim(-100,6000) +
  labs(x = "Topic", y = "Count",
  title = "Number of Tweets most associated with Each 
  LSA Topic for #depression")
dep_lsa_term <- lsa_func(deplsa_topics$tk)
ggplot(dep_lsa_term) + geom_bar(aes(x = word, y = freq, 
  fill = factor(label)), stat = "identity", show.legend = FALSE) +
  facet_wrap(~label,scales = "free") +
  coord_flip() + labs(x = "Words", y = "LSA Similarity Score", 
  title = "Top 5 Words per LSA Topic for #depression")

suitdm_sparse_lsa <- removeSparseTerms(stdm, sparse = 0.99)
suitdm_tfidf_lsa <- weightTfIdf(suitdm_sparse_lsa)
suilsa_topics <- lsa(suitdm_tfidf_lsa, dims = 10)
docu_sui <- as.data.frame(suilsa_topics$dk)
colnames(docu_sui) <- c("TopicA", "TopicB", "TopicC", "TopicD",
  "TopicE", "TopicF", "TopicG", "TopicH", "TopicI", "TopicJ")
doc_topics_sui <- as.data.frame(colnames(docu_sui)[apply(docu_sui,1,
  which.max)])
colnames(doc_topics_sui) <- "Topic"
df.add_sui <- data.frame(Topic = "TopicA", n = 0)
doc_topics_count_sui <- doc_topics_sui %>% dplyr::count(Topic) %>%
  as.data.frame() %>% bind_rows(df.add_sui)
ggplot(doc_topics_count_sui) + 
  geom_bar(aes(x = factor(Topic), y = n, fill = factor(Topic)), 
  stat = "identity", show.legend = FALSE) + 
  geom_text(aes( x = factor(Topic), y = n, label = n),
  stat = "identity", vjust = -1) + ylim(-100,6000) +
  labs(x = "Topic", y = "Count",
  title = "Number of Tweets most associated with Each 
  LSA Topic for #suicide")
sui_lsa_term <- lsa_func(suilsa_topics$tk)
ggplot(sui_lsa_term) + geom_bar(aes(x = word, y = freq, 
  fill = factor(label)), stat = "identity", show.legend = FALSE) +
  facet_wrap(~label,scales = "free") +
  coord_flip() + labs(x = "Words", y = "LSA Similarity Score", 
  title = "Top 5 Words per LSA Topic for #suicide")