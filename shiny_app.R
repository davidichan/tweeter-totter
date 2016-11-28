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
      textInput("myselect", "Enter twitter username (no @)", "hspter"),
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
        tabPanel("Twitter Ideology Fit", plotOutput("myplot")),
        tabPanel("Table", dataTableOutput("mytable"))
      )
    )
  )
    )
server <- function(input, output, session) { 
  f1 <- reactive({
    #Modify in order to a) store new friend lists and b) look up friends that already exist
    getFriends(screen_name=input$myselect, oauth_folder="~/Documents/Big_Data/t-t/credentials")
    })
  id1 <- reactive({
    #ideol_est <- estimateIdeology(input$myselect, f1(), method="MLE")
    estimateIdeology(input$myselect, f1(), method="MLE")
  })
    
#    if(input$myselect == "user_hspter"){
 #     friends <- friends_hspter}
  #  else {
   #   friends <- friends_hc
  #  }
  
  t1 <- reactive({
    renderTable({
      data.frame(
        UserName = input$myselect,
        Theta = (summary(id1()))[2,1])
    })
  })
  #Need to figure out how to make results a variable, but already friends is a variable so 
  #not sure how to keep them both as variables

  observe({
   #friends <- f1()
   #ideol_est <- id1()
    output$myplot <- renderPlot({plot(id1())
      #Change plot to be a straight line through some point (0, some-y) and slope of -theta
    })  
    output$mytable <- renderDataTable({economics[,c("date", input$myselect)]})
    output$values <- t1()
  })
}

shinyApp(ui = ui, server = server)