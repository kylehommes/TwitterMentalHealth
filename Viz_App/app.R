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

load("Twitter_viz_app.RData")

# Choices for the Input Selection
Hashtag = list("#MentalHealthAwareness" = "m",
               "#Anxiety" = "a","#Suicide" = "s","#Depression" = "d",
               "#PTSD" = "p","All Hashtags" = "t")

Sent_choice = list("Postive/Negative Sentiment Segments Plot" = 
                     "bing_seg_plot","Most Frequent Sentiment Words Plot" = "bing_plot", 
                   "Positive/Negative Sentiment by Hashtag" = "bing_hashtag_plot", 
                   "Sentiment Groups Most Frequent Words Plot" = "nrc_plot",
                   "Sentiment Groups by Hashtag" = "nrc_hashtag_plot")

Topics_choice = list("MHA LSA Plot" = "mha_lsa_plot", 
                     "MHA LDA Plot" = "mha_lda_words_plot",
                     "MHA Cluster Plot" = "mha_clust_words_plot",
                     "Anxiety LSA Plot" = "anx_lsa_plot", 
                     "Anxiety LDA Plot" = "anx_lda_words_plot",
                     "Anxiety Cluster Plot" = "anx_clust_words_plot",
                     "Depression LSA Plot" = "dep_lsa_plot", 
                     "Depression LDA Plot" = "dep_lda_words_plot",
                     "Depression Cluster Plot" = "dep_clust_words_plot",
                     "PTSD LSA Plot" = "ptsd_lsa_plot", 
                     "PTSD LDA Plot" = "ptsd_lda_plot",
                     "PTSD Cluster Plot" = "ptsd_clust_words_plot",
                     "Suicide LSA Plot" = "sui_lsa_plot", 
                     "Suicide LDA Plot" = "sui_lda_words_plot",
                     "Suicide Cluster Plot" = "sui_clust_words_plot",
                     "All Hashtags LSA Plot" = "tot_lsa_words_plot", 
                     "All Hashtags LDA Plot" = "tot_lda_words_plot",
                     "All Hashtags Cluster Plot" = "tot_clust_words_plot",
                     "LDA Topics" = "lda_plot", 
                     "LSA Topics" = "lsa_plot", 
                     "Cluster Topics" = "cluster_plot",
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
  })  
  })   
  })  
  })
  })
}

shinyApp(ui = ui, server = server)

