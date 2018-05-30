#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Twitter Mental Health Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("words",
                     "Number of Words:",
                     min = 10,
                     max = 150,
                     value = 75)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     wordcloud(mhafreq.df$word,mhafreq.df$freq,max.words=input$words,
        colors=brewer.pal(8,"Dark2"),scale=c(7,0.15),random.order=F)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

