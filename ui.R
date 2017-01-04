library(shiny)
library(tweetscores)


shinyUI <- basicPage(
  tags$head(
    tags$style(HTML("
                    h1{color:black;
	        	font-style:italic;}
                    h4{
                    color:grey;
                    font-style:italic;
                    }
                    "))
    ),

  h1("Tweeter-totter"),
  h4("Find balance in your tweets."),
  h4("Enter your twitter user name below and get an estimate of your twitter feed ideology. Check out some of the twitter accounts you could follow to balance-out your feed in the tables below."),

  sidebarLayout(
    sidebarPanel("",
      textInput("myselect", "Enter a twitter username (no @)", "neiltyson"),
      #verbatimTextOutput("value")
      tableOutput("values"),
      submitButton("Run Query")
),

    mainPanel(
      helpText("See where your twitter account lies on the ideological spectrum"),
      tabPanel("Twitter Ideology Fit", plotOutput("myplot")),
      helpText("Consider following some of these accounts to help balance your feed"),
      tabsetPanel(
        tabPanel("Twitter elites to consider following", dataTableOutput("elitestable")),
        tabPanel("Some twitter mortals to consider following", dataTableOutput("mytable"))),
      helpText("Built using the ", a("tweetscores package", href="https://github.com/pablobarbera/twitter_ideology")),
      helpText("Questions? Tweet me ", a("@dch4n", href="https://twitter.com/dch4n"))
          )
  )
)

