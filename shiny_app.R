library(ggplot2)
library(shiny)
library(tweetscores)
ui <- basicPage(
  
  tags$head(
    tags$style(HTML("
                    h1{color:blue;}
                    h4{
                    color:grey;
                    font-style:italic;
                    }
                    "))
    ),
  
  h1("Twitter ideology"),
  h4("Estimating twitter ideology based on accounts followed"),
  
  sidebarLayout(
    
    sidebarPanel(
      "My sidebar",
      textInput("myselect", "Caption", "Data Summary"),
      #verbatimTextOutput("value")
      tableOutput("values")
      ),
      
      
#      selectInput("myselect", label = "Select variable",
#                  choices = c("User1" = "user_hspter", "User2" = "user_hc"),
#                  selected = "user_hspter"),
#      tableOutput("values")
#    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("blah", plotOutput("myplot")),
        tabPanel("Table", dataTableOutput("mytable"))
      )
    )
  )
    )
server <- function(input, output, session) { 
  f1 <- reactive({
    if(input$myselect == "user_hspter"){
      friends <- friends_hspter}
    else {
      friends <- friends_hc
    }
  })
  t1 <- reactive({
    renderTable({
      data.frame(
        Name=input$myselect,
        Value= (summary(results_hc))[2,1])
    })
  })
  #Need to figure out how to make results a variable, but already friends is a variable so 
  #not sure how to keep them both as variables

  observe({
    friends <- f1()
    output$myplot <- renderPlot({plot(estimateIdeology(input$myselect, friends, method="MLE"))
    })  
    output$mytable <- renderDataTable({economics[,c("date", input$myselect)]})
    output$values <- t1()
  })
}

shinyApp(ui = ui, server = server)