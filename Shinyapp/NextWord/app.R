#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinythemes)

source("Capstone_Prediction_Function.R")



#fourGram <- filter(fourGram, Frequency > 1)
#triGram <- filter(triGram, Frequency > 1)
#biGram <- filter(biGram, Frequency > 1)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
   
   # Application title
   titlePanel("Predicting the next word"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("inputwords", "Enter any number of words here:"),
         h5("The algorithm takes several seconds to run. Please be patient.")
      ),
      
      
      # Show the next word
      mainPanel(
                h4("Next word:"),
                 h3(textOutput("prediction"))
 #               h4(" "),
 #               h4(" "),
 #               h4("The table that will show up here displays the top 5 predicted words and how frequently the occur in the data:"),
 #               tableOutput("wordtable")
      )
   )
)


server <- function(input, output) {
   
   output$prediction <- renderText({

      x <- input$inputwords 
      x <- fun.predict(x)
      
     
     x
   })
#   output$wordtable <- renderTable({
#     y <- input$inputwords
#     y <- fun.predict(y)
     
#     head(y, 5)
#   })
 
}


# Run the application 
shinyApp(ui = ui, server = server)

