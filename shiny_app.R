library(ggplot2)
library(shiny)
library(tweetscores)
#devtools::session_info('DT')

addfriends <- function(usrname, fr_list=friends.list){
  fr <- getFriends(screen_name=usrname, oauth_folder="~/Documents/Big_Data/t-t/credentials")
  fr_list <- append(fr_list, list(tempname = fr))
  names(fr_list)[names(fr_list) == "tempname"] <- usrname
  return(fr_list)
}

plot.estId2 <- function(x, user){
  # loading reference data
  data <- tweetscores::refdata
  # adding user data
  theta <- x
  twit_name <- c(paste0("@", user))
  data <- rbind(data, c(twit_name, theta, theta, theta))
  data$phi <- as.numeric(data$phi)
  data$phi.lo <- as.numeric(data$phi.lo)
  data$phi.hi <- as.numeric(data$phi.hi)
  # preparing plot
  p <- ggplot(data, aes(y=reorder(screenName, -phi), x=phi))
  pq <- p + geom_point(size=1.25) +
    geom_point(data=data[data$screenName==twit_name,], colour="firebrick1", size=3) +
    geom_segment(width=.5, aes(x=phi.lo, xend=phi.hi, y=reorder(screenName, -phi),
                               yend=reorder(screenName, -phi)), position=position_dodge(.5)) +
    theme_bw() + scale_y_discrete("") +
    scale_x_continuous(expression(paste("Estimated Ideological Ideal Points")),
                       lim=range(data[,2:4]))
  suppressMessages(suppressWarnings(print(pq)))
}

ui <- basicPage(
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
server <- function(input, output, session) { 
  ### Gets friends list for calculation of ideal position
  f1 <- reactive({
    #Modify in order to a) store new friend lists and b) look up friends that already exist
    #getFriends(screen_name=input$myselect, oauth_folder="~/Documents/Big_Data/t-t/credentials")
    if(input$myselect %in% names(friends.list)){
        #if name is in list, then use friends list in here
        friends.list[[input$myselect]]
      } else {
        #if name is not in list, then create a new entry in the list
        friends.list <<- addfriends(input$myselect, friends.list)
        friends.list[[input$myselect]]
      }
    })
  ### Gets ideal ideology based on friends list in f1()
  id1 <- reactive({
    #ideol_est <- estimateIdeology(input$myselect, f1(), method="MLE")
    #estimateIdeology(input$myselect, f1(), method="MLE")
    user.score <- tryCatch({
      estimateIdeology2(input$myselect, f1())
    }, error = function(e){
        print("Account doesn't follow any twitter elites")
      })
    if(!input$myselect %in% elites.df$names){
      if(user.score > 0 & !input$myselect %in% mortals.right$names){
        mortals.right <<- rbind(mortals.right, data.frame(names=input$myselect, scores=user.score))
      } else if(user.score <0 & !input$myselect %in% mortals.left$names){
        mortals.left <<- rbind(mortals.left, data.frame(names=input$myselect, scores=user.score))
      }
    }
    return(user.score)
  })
  
  ### Gets ideal value Theta for user 
  t1 <- reactive({
    renderTable({
      data.frame(
        UserName = input$myselect,
        #Theta = (summary(id1()))[2,1])
        "Twitter ideology score" = id1())
    })
  })
  
  ### Creates teeter-totter plot
# pl1 <- reactive({
#   plot.estId2(id1(), input$myselect)
#   })
  
  tb1 <- reactive({
    if(id1() > 0){
      indx <- sample(dim(elites.left)[1],size=5)
      temp.df <- elites.left[indx,]
      Username <- paste0("<a href='https://twitter.com/",temp.df[,1],"'>",temp.df[,1],"</a>")
      renderDataTable({cbind(Username, elites.left[indx,2])}, escape = FALSE, options = list(dom = ''))
      #renderTable({elites.left[sample(dim(elites.left)[1], size=5),]})
    } else if(id1() <0){
      indx <- sample(dim(elites.right)[1],size=5)
      temp.df <- elites.right[indx,]
      Username <- paste0("<a href='https://twitter.com/",temp.df[,1],"'>",temp.df[,1],"</a>")
      renderDataTable({cbind(Username, elites.right[indx,2])}, escape = FALSE, options = list(dom = ''))
      #renderTable({elites.right[sample(dim(elites.right)[1], size=5),]})
    }
  })
  
  tb2 <- reactive({
    if(id1() > 0){
      indx <- sample(dim(mortals.left)[1],size=5)
      temp.df <- mortals.left[indx,]
      #links <- paste0("https://twitter.com/",temp.df[,1])
      Username <- paste0("<a href='https://twitter.com/",temp.df[,1],"'>",temp.df[,1],"</a>")
      renderDataTable({cbind(Username, mortals.left[indx,2])}, escape = FALSE, options = list(dom = ''))
    } else if(id1() <0){
      indx <- sample(dim(mortals.right)[1],size=5)
      temp.df <- mortals.right[indx,]
      #links <- paste0("https://twitter.com/",temp.df[,1])
      Username <- paste0("<a href='https://twitter.com/",temp.df[,1],"'>",temp.df[,1],"</a>")
      renderDataTable({cbind(Username, mortals.right[indx,2])}, escape = FALSE, options = list(dom = ''))
    }
  })

  observe({
    output$myplot <- renderPlot({
      #pl1()
      plot.estId2(id1(), input$myselect)
      })
      #plot(id1())})
      #Change plot to be a straight line through some point (0, some-y) and slope of -theta
    output$mytable <- tb2()
    output$elitestable <- tb1()
    
    output$values <- t1()
  })
}

shinyApp(ui = ui, server = server)