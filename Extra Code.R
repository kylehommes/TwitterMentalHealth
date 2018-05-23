df.add <- data.frame(Topic = "TopicB", n = 0)
doc_topics_count <- doc_topics %>% dplyr::count(Topic) %>%
  as.data.frame() %>% bind_rows(df.add)
ggplot(doc_topics_count) + 
  geom_bar(aes(x = factor(Topic), y = n, fill = factor(Topic)), 
           stat = "identity", show.legend = FALSE) + 
  geom_text(aes( x = factor(Topic), y = n, label = n),
            stat = "identity", vjust = -1) + ylim(-1000,40000) +
  labs(x = "Topic", y = "Count", 
       title = "Number of Tweets most associated with Each LSA Topic")