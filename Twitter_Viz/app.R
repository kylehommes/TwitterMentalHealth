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

load("Twitter.RData")

# Choices for the Input Selection
Hashtag = list("#MentalHealthAwareness" = "m",
  "#Anxiety" = "a","#Suicide" = "s","#Depression" = "d",
  "#PTSD" = "p","All Hashtags" = "t")

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Twitter Mental Health Analysis"),
   
   # Sidebar with a slider input for number of words
   sidebarLayout(
      sidebarPanel(
          selectInput("selection", "Choose Hashtag:",
                      choices = Hashtag),
          hr(),
        sliderInput("words",
                     "Number of Words:",
                     min = 10,
                     max = 150,
                     value = 75)
      ),
      
      # Show a plot of the wordcloud
      mainPanel(
         plotOutput("wordcloud")
      )
   )
)

server <- function(input, output) {
  output$wordcloud <- renderPlot({
     wordcloud_rep = repeatable(wordcloud)
     data <- get(input$selection)
     wordcloud_rep(data$word,data$freq,
        max.words=input$words,colors=brewer.pal(8,"Dark2"),
        scale=c(7,.01),random.order=F)
  })
}

shinyApp(ui = ui, server = server)

