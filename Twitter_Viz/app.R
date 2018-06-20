#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(tm)
library(twitteR)
library(tidyr)
library(factoextra)
library(gridExtra)
library(RColorBrewer)
library(maps)
library(tidytext)
library(reshape2)
library(memoise)

load("~/Desktop/Twitter.RData")

tweet_search <- totaltweet %>% dplyr::mutate(rowIndex=
  as.numeric(row.names(.))) %>%
  dplyr::select(retweetCount,rowIndex,hashtag)
tweet_search$text <- tottxt
docList <- as.list(tweet_search$text)
N.docs <- length(docList)
QrySearch <- memoise(function(queryTerm) {
  my.docs <- VectorSource(c(docList, queryTerm))
  my.corpus <- VCorpus(my.docs) %>% 
    tm_map(stemDocument) %>%
    tm_map(removeNumbers) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords,stopwords("en")) %>%
    tm_map(stripWhitespace)
  term.doc.matrix.stm <- TermDocumentMatrix(my.corpus,
    control=list(weighting=function(x) weightSMART(x,spec="ltc"),
    wordLengths=c(1,Inf)))
  term.doc.matrix <- tidy(term.doc.matrix.stm) %>% 
    dplyr::group_by(document) %>% 
    dplyr::mutate(vtrLen=sqrt(sum(count^2))) %>% 
    dplyr::mutate(count=count/vtrLen) %>% 
    ungroup() %>% 
    dplyr::select(term:count)
  docMatrix <- term.doc.matrix %>% 
    dplyr::mutate(document=as.numeric(document)) %>% 
    dplyr::filter(document<N.docs+1)
  qryMatrix <- term.doc.matrix %>% 
    dplyr::mutate(document=as.numeric(document)) %>% 
    dplyr::filter(document>=N.docs+1)
  searchRes <- docMatrix %>% 
    inner_join(qryMatrix,by=c("term"="term"),
    suffix=c(".doc",".query")) %>% 
    dplyr::mutate(termScore=round(count.doc*count.query,4)) %>% 
    dplyr::group_by(document.query,document.doc) %>% 
    dplyr::summarise(Score=sum(termScore)) %>% 
    filter(row_number(desc(Score))<=10) %>% 
    dplyr::arrange(desc(Score)) %>% 
    left_join(tweet_search,by=c("document.doc"="rowIndex")) %>% 
    ungroup() %>% 
    rename(Result=text) %>% 
    dplyr::select(Result,Score,retweetCount,hashtag) %>% 
    data.frame()
  return(searchRes)
})

# Choices for the Input Selection
Hashtag = list("#MentalHealthAwareness" = "m",
               "#Anxiety" = "a","#Suicide" = "s","#Depression" = "d",
               "#PTSD" = "p","All Hashtags" = "t")

Sent_choice = list("Postive/Negative Sentiment Segments Plot" = 
                     "bing_seg_plot","Most Frequent Sentiment Words Plot" = "bing_plot", 
                   "Positive/Negative Sentiment by Hashtag" = "bing_hashtag_plot", 
                   "Sentiment Groups Most Frequent Words Plot" = "nrc_plot",
                   "Sentiment Groups by Hashtag" = "nrc_hashtag_plot")

Topics_choice = list("LDA Topics" = "lda_plot", 
                     "LSA Topics" = "lsa_plot", "Cluster Topics" = "cluster_plot",
                     "Bigrams" = "bigram_plot", "Trigrams" = "trigram_plot", 
                     "4-grams" = "fourgram_plot")

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Twitter Mental Health Analysis"),
  
  # Sidebar with a slider input for number of words
  tabsetPanel(
    tabPanel("Sentiment", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("selection_sent", "Choose Plot:",
                             choices = Sent_choice)),
               mainPanel(
                 plotOutput("sentiment")
               )
             )
    ),
    
    tabPanel("Topic Modeling", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("selection_topic", "Choose Method:",
                             choices = Topics_choice)),
               mainPanel(
                 plotOutput("topic")
               )
             )
    ),
    
    tabPanel("Location", fluid = TRUE,
             sidebarLayout(
               sidebarPanel("Location of the Tweets in 
                            #mentalheathawareness"),
               mainPanel(
                 plotOutput("location")
               )
               )
    ),
    
    tabPanel("Wordcloud", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("selection", "Choose Hashtag:",
                             choices = Hashtag),
                 hr(),
                 sliderInput("words",
                             "Number of Words:",
                             min = 10,
                             max = 150,
                             value = 75)),
               mainPanel(
                 plotOutput("wordcloud", width = "100%", height = "800")  
               )
             )
    ),
    
    tabPanel("Comparison Cloud", fluid = TRUE,
             sidebarLayout(
               sidebarPanel("Comparison of Positive and Negative
                            Sentiment Words in Wordcloud"),
               mainPanel(
                 plotOutput("compare", width = "100%", height = "800")
               )
               
               )
  ),
  tabPanel("Search", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               textInput("text", "Word to Search:"),
               actionButton(inputId = "submit_loc", label = "Submit")),
             mainPanel(
               tableOutput("search")
             )
           )
  )
  )
  )

server <- function(input, output) {
  output$location <- renderPlot({
    worldMap <- map_data("world")
    loc_plot <- ggplot(worldMap)
    map_plot <- loc_plot + geom_path(aes(x = long, y = lat, 
                                         group = group)) +
      geom_point(data = location,aes(x=lon,y=lat),color="RED",
                 size=.25) + labs(x = "Longitude", y = "Latitude", 
                                  title = "Map of Location for #mentalhealthawarness Tweets")
    print(map_plot)
    
    output$wordcloud <- renderPlot({
      wordcloud_rep = repeatable(wordcloud)
      data <- get(input$selection)
      wordcloud_rep(data$word,data$freq,
                    max.words=input$words,colors=brewer.pal(8,"Dark2"),
                    scale=c(10,.05),random.order=F)
      
      output$sentiment <- renderPlot({
        sent_plot <- get(input$selection_sent)
        print(sent_plot)
        
        output$topic <- renderPlot({
          topic_plot <- get(input$selection_topic)
          print(topic_plot)
          output$compare <- renderPlot({
            print(tidytweet %>%
                    inner_join(get_sentiments("bing")) %>%
                    dplyr::count(word, sentiment, sort = TRUE) %>%
                    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
                    comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                                     max.words = 100,scale=c(8,.5)))
            difference = eventReactive(input$submit_loc,{
              QrySearch(input$text)})
            output$search <- renderTable({
              difference()
            })  
          })  
        })   
      })  
    })
  })
}

shinyApp(ui = ui, server = server)

