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

load("Search.RData")

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

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Twitter Mental Health Analysis - Search"),
      
   # Sidebar with a slider input for number of words
   
              sidebarLayout(
                sidebarPanel(
                  textInput("text", "Word to Search:"),
                  actionButton(inputId = "submit_loc", label = "Submit")),
                mainPanel(
                  tableOutput("search")
                )
              )
     )

server <- function(input, output) {
 
  difference = eventReactive(input$submit_loc,{
    QrySearch(input$text)})
  output$search <- renderTable({
    difference()
  })  
}

shinyApp(ui = ui, server = server)

