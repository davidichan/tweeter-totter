library(shiny)
library(tweetscores)


shinyUI <- basicPage(
  tags$head(
    tags$style(HTML("
                    h1{color:black;}
                    h4{
                    color:grey;
                    font-style:italic;
                    }
                    "))
    ),

  h1("Tweeter-totter"),
  h4("Find balance in your tweets"),

  sidebarLayout(
    sidebarPanel("",
      textInput("myselect", "Enter a twitter username (no @)", "neiltyson"),
      #verbatimTextOutput("value")
      tableOutput("values")),

    mainPanel(
      helpText("See where your twitter account lies on the ideological spectrum"),
      tabPanel("Twitter Ideology Fit", plotOutput("myplot")),
      helpText("Consider following some of these accounts to help balance your feed"),
      tabsetPanel(
        tabPanel("Twitter elites to consider following", dataTableOutput("elitestable")),
        tabPanel("Some twitter mortals to consider following", dataTableOutput("mytable"))),
      helpText("Built using the ", a("tweetscores package", href="https://github.com/pablobarbera/twitter_ideology")),
      helpText("Questions? Email tweeter.totter.xyz@gmail.com")
          )
  )
)

