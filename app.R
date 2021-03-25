#Ref- https://github.com/rstudio/shiny-examples/blob/master/030-basic-datatable/ui.R
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#source("you_tube_search_rank.R")
library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Basic DataTable"),
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(3, 
               textInput("text", h3("Search YouTube"), 
                         value = "Panner Butter Masala")) ,
        
            column(4,
               selectInput("viewcount",
                           "Viewers Count:",
                           c("All",
                             unique(as.character(my_df$viewcount))))
        ),
        column(4,
               selectInput("likecount",
                           "Like Count:",
                           c("All",
                             unique(as.character(my_df$likecount))))
        ),
        column(4,
               selectInput("dislikecount",
                           "Dislike Count:",
                           c("All",
                             unique(as.character(my_df$dislikecount))))
        )
    ),
    # Create a new row for the table.
    DT::dataTableOutput("table")
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
#       my_df <- my_yt_search(input$text)
        
                        data <- my_df[,c(1,2,3,4,6,10,11)]
        if (input$viewcount != "All") {
            data <- data[data$viewcount == input$viewcount,]
        }
        if (input$dislikecount != "All") {
            data <- data[data$dislikecount == input$dislikecount,]
        }
        if (input$likecount != "All") {
            data <- data[data$likecount == input$likecount,]
        }
        data
        
        
        
    }))
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
