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

ggplot(tweet_combo_lsa) + 
  geom_bar(aes(x = factor(Topic), fill = factor(Topic)), 
           show.legend = FALSE) + 
  geom_text(aes( x = factor(Topic), label = ..count..),
            stat = "count", vjust = -1) + ylim(-1000,40000) +
  labs(x = "Topic", y = "Count", 
       title = "Number of Tweets most associated 
       with Each LSA Topic by Hashtag") + 
  facet_wrap(~hashtag, scales = "free")

output$distPlot <- renderPlot({
  # generate bins based on input$bins from ui.R
  x    <- faithful[, 2] 
  bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  # draw the histogram with the specified number of bins
  hist(x, breaks = bins, col = 'darkgray', border = 'white')
})

OtherHashtag = list("#MentalHealthAwareness" = "mm",
                    "#Anxiety" = "aa","#Suicide" = "ss","#Depression" = "dd",
                    "#PTSD" = "pp","All Hashtags" = "tt")

dofunc <- function(x) {
  if (x == 1){
    return(m)
  }
  else if (x == 2){
    return(a)
  }
  else if (x == 3){
    return(s)
  }
  else if (x == 4){
    return(d)
  }
  else if (x == 5){
    return(p)
  }
  else if (x == 6){
    return(t)
  }
  else print("Unknown Hashtag")
}

dofunc2 <- function(x) {
  if (x == 1){
    return(mm)
  }
  else if (x == 2){
    return(aa)
  }
  else if (x == 3){
    return(ss)
  }
  else if (x == 4){
    return(dd)
  }
  else if (x == 5){
    return(pp)
  }
  else if (x == 6){
    return(tt)
  }
  else print("Unknown Hashtag")
}