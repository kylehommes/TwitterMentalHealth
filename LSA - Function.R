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