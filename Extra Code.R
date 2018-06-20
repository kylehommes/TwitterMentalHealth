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
  
  tabPanel("Comparison Cloud", fluid = TRUE,
           sidebarLayout(
             sidebarPanel("Comparison of Positive and Negative
                          Sentiment Words in Wordcloud"),
             mainPanel(
               plotOutput("compare")
             )
             )
           )
  output$compare <- renderPlot({
    tidytweet %>%
      inner_join(get_sentiments("bing")) %>%
      dplyr::count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 100,scale=c(2,.05))
  })
  
  
  tabPanel("Topic Modeling", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               selectInput("selection_topic", "Choose Method:",
                           choices = Topics_choice)),
             mainPanel(
               plotOutput("topic")
             )
           )
  )
  
  
  output$topic <- renderPlot({
    topic_plot <- get(input$selection_topic)
    print(topic_plot)
  })
  
}

tweet_search <- totaltweet %>% dplyr::mutate(rowIndex=
  as.numeric(row.names(.))) %>%
  dplyr::select(retweetCount,rowIndex,hashtag)
tweet_search$text <- tottxt
docList <- as.list(tweet_search$text)
N.docs <- length(docList)  

# Libraries needed for the app

# Load the data
location <- read.csv("location.csv")
m <- read.csv("mhafreq.csv")
a <- read.csv("~/Desktop/afreq.csv")
s <- read.csv("~/Desktop/sfreq.csv")
d <- read.csv("dfreq.csv")
p <- read.csv("pfreq.csv")
t <- read.csv("totfreq.csv")
mhatweet <- read.csv("mhatweet.csv")
anxtweet <- read.csv("anxtweet.csv")
suitweet <- read.csv("mhatweet.csv")
ptsdtweet <- read.csv("anxtweet.csv")
deptweet <- read.csv("mhatweet.csv")
totaltweet <- read.csv("anxtweet.csv")

# Choices for the Input Selection
Hashtag = list("#MentalHealthAwareness" = "m",
               "#Anxiety" = "a","#Suicide" = "s","#Depression" = "d",
               "#PTSD" = "p","All Hashtags" = "t")

totaltweet <- read.csv("totaltweet.csv", header = TRUE)
tweet_search <- totaltweet %>% dplyr::mutate(rowIndex=
  as.numeric(row.names(.))) %>%
  dplyr::select(retweetCount,rowIndex,hashtag)
tweet_search$text <- tottxt
docList <- as.list(tweet_search$text)
N.docs <- length(docList)

term_topics_m <- as.data.frame(mhalsa_topics$tk)
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
term_topics3_m <- term_topics_m %>% select(TopicC,word) %>%
  arrange(desc(TopicC)) %>% mutate(label = "TopicC",
                                   freq = TopicC) %>% as.data.frame()
term_topics3_m <- term_topics3_m[c(1:5),]
term_topics4_m <- term_topics_m %>% select(TopicD,word) %>%
  arrange(desc(TopicD)) %>% mutate(label = "TopicD",
                                   freq = TopicD) %>% as.data.frame()
term_topics4_m <- term_topics4_m[c(1:5),]
term_topics5_m <- term_topics_m %>% select(TopicE,word) %>%
  arrange(desc(TopicE)) %>% mutate(label = "TopicE",
                                   freq = TopicE) %>% as.data.frame()
term_topics5_m <- term_topics5_m[c(1:5),]
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

d <- dist(t(totlsa_topics_clust$tk), method="manhattan")
d <- data.table(d)