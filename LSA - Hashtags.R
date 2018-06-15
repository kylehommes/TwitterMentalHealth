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
            stat = "identity", vjust = -1) + ylim(-1000,40000) +
  labs(x = "Topic", y = "Count",
       title = "Number of Tweets most associated with Each 
       LSA Topic for #mentalhealthawarness")
term_topics_m <- as.data.frame(totlsa_topics_m$tk)
colnames(term_topics_m) <- c("TopicA", "TopicB", "TopicC", "TopicD",
                             "TopicE", "TopicF", "TopicG", "TopicH", "TopicI", "TopicJ")
term_top_list_m <- 
  as.data.frame(colnames(term_topics_m)[apply(term_topics_m,
                                              1,which.max)])
colnames(term_top_list_m) <- "Topic"
term_topics_m$word <- row.names(term_topics_m)
term_topics_m <- term_topics_m %>% bind_cols(term_top_list)
term_topics1_m <- term_topics_m %>% select(TopicA,word) %>%
  arrange(desc(TopicA)) %>% mutate(label = "TopicA",
                                   freq = TopicA) %>% as.data.frame()
term_topics1_m <- term_topics1_m[c(1:5),]
term_topics2_m <- term_topics_m %>% select(TopicB,word) %>%
  arrange(desc(TopicB)) %>% mutate(label = "TopicB",
                                   freq = TopicB) %>% as.data.frame()
term_topics2_m <- term_topics2_m[c(1:5),]
term_topics3 <- term_topics %>% select(TopicC,word) %>%
  arrange(desc(TopicC)) %>% mutate(label = "TopicC",
                                   freq = TopicC) %>% as.data.frame()
term_topics3 <- term_topics3[c(1:5),]
term_topics4 <- term_topics %>% select(TopicD,word) %>%
  arrange(desc(TopicD)) %>% mutate(label = "TopicD",
                                   freq = TopicD) %>% as.data.frame()
term_topics4 <- term_topics4[c(1:5),]
term_topics5 <- term_topics %>% select(TopicE,word) %>%
  arrange(desc(TopicE)) %>% mutate(label = "TopicE",
                                   freq = TopicE) %>% as.data.frame()
term_topics5 <- term_topics5[c(1:5),]
term_topics6 <- term_topics %>% select(TopicF,word) %>%
  arrange(desc(TopicF)) %>% mutate(label = "TopicF",
                                   freq = TopicF) %>% as.data.frame()
term_topics6 <- term_topics6[c(1:5),]
term_topics7 <- term_topics %>% select(TopicG,word) %>%
  arrange(desc(TopicG)) %>% mutate(label = "TopicG",
                                   freq = TopicG) %>% as.data.frame()
term_topics7 <- term_topics7[c(1:5),]
term_topics8 <- term_topics %>% select(TopicH,word) %>%
  arrange(desc(TopicH)) %>% mutate(label = "TopicH",
                                   freq = TopicH) %>% as.data.frame()
term_topics8 <- term_topics8[c(1:5),]
term_topics9 <- term_topics %>% select(TopicI,word) %>%
  arrange(desc(TopicI)) %>% mutate(label = "TopicI",
                                   freq = TopicI) %>% as.data.frame()
term_topics9 <- term_topics9[c(1:5),]
term_topics10 <- term_topics %>% select(TopicJ,word) %>%
  arrange(desc(TopicJ)) %>% mutate(label = "TopicJ",
                                   freq = TopicJ) %>% as.data.frame()
term_topics10 <- term_topics10[c(1:5),]
term_topics_tot <- bind_rows(term_topics1,term_topics2,term_topics3,
                             term_topics4,term_topics5,term_topics6,term_topics7,term_topics8,
                             term_topics9,term_topics10)
ggplot(term_topics_tot) + geom_bar(aes(x = word, y = freq, 
                                       fill = factor(label)), stat = "identity", show.legend = FALSE) +
  facet_wrap(~label,scales = "free") +
  coord_flip() + labs(x = "Words", y = "LSA Similarity Score", 
                      title = "Top 5 Words per LSA Topic")