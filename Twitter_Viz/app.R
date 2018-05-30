#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Twitter Mental Health Analysis"),
   
   # Sidebar with a slider input for number of words
   sidebarLayout(
      sidebarPanel(
          selectInput("selection", "Choose a Hashtag:",
                      choices = Hashtag),
          actionButton("update", "Change"),
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
   reactive({
    input$update
    isolate({
      withProgress({
      dofunc(input$selection)
      })
    })
  }) 
  output$wordcloud <- renderPlot({
     wordcloud_rep = repeatable(wordcloud)
     wordcloud_rep(a,b,
        max.words=input$words,colors=brewer.pal(8,"Dark2"),
        scale=c(3,.1),random.order=F)
   })
}

shinyApp(ui = ui, server = server)

