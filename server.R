library(ggplot2)
library(shiny)
library(tweetscores)

rootp <- ""
credentials_folder <- "credentials"
load(paste0(rootp,"mortals_left.RData"))
load(paste0(rootp,"mortals_right.RData"))
load(paste0(rootp,"elites.RData"))
load(paste0(rootp,"friends_list.RData"))
load(paste0(rootp,"user_info.RData"))

addfriends <- function(usrname, fr_list=friends.list){
  fr <- getFriends(screen_name=usrname, oauth_folder=credentials_folder)
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

serverServer <- function(input, output, session) {
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
	      save(friends.list, file=(paste0(rootp,"friends_list.RData")))
        friends.list[[input$myselect]]
      }
    })
  ### Gets ideal ideology based on friends list in f1()
  id1 <- reactive({
    #ideol_est <- estimateIdeology(input$myselect, f1(), method="MLE")
    #estimateIdeology(input$myselect, f1(), method="MLE")
    user.score <- tryCatch({
      ### Gets user info for determining whether to get friends and for table output
      if(input$myselect %in% user.info$screen_name){
        #if name is in list, then don't do anything
      } else {
        #if name is not in list, then create a new entry in the list
        temp.info <<- getUsersBatch(screen_names = input$myselect, oauth_folder = credentials_folder)
        user.info <<- rbind(user.info, temp.info)
        save(user.info, file=(paste0(rootp,"user_info.RData")))
      }
      score <- estimateIdeology2(input$myselect, f1())
       round(score,2)
    }, error = function(e){
        print("Cannot estimate ideology.")
      })
    if(!input$myselect %in% elites.df$names){
      if(user.score > 0 & !input$myselect %in% mortals.right$names &! is.character(user.score)){
        mortals.right <<- rbind(mortals.right, data.frame(names=input$myselect, scores=user.score))
        save(mortals.right, file=(paste0(rootp,"mortals_right.RData")))
      } else if(user.score <0 & !input$myselect %in% mortals.left$names &! is.character(user.score)){
        mortals.left <<- rbind(mortals.left, data.frame(names=input$myselect, scores=user.score))
	      save(mortals.left, file=(paste0(rootp,"mortals_left.RData")))
      }
    }
    return(user.score)
  })

  ### Creates output of ideal value 
  t1 <- reactive({
    if(is.character(id1())){
      statement <- id1()
    } else {
      statement <- format(round(id1(), 2), nsmall = 2)
    }
    renderTable({
      data.frame(
        UserName = input$myselect,
        #Theta = (summary(id1()))[2,1])
        #"Twitter ideology score" = format(round(id1(), 2), nsmall = 2))
        "TwitterIdeologyScore" = statement)
    })
  })
 tb1 <- reactive({
    if(id1() > 0){
      indx <- sample(dim(elites.left)[1],size=5)
      temp.df <- elites.left[indx,]
      temp.df <- temp.df[order(temp.df$scores, decreasing = T),]
      Username <- paste0("<a href='https://twitter.com/",temp.df[,1],"'>",temp.df[,1],"</a>")
      renderDataTable({cbind(Username, Score = format(round(temp.df[,2], 2), nsmall = 2), Description = temp.df[,5])}, escape = FALSE, options = list(dom = 't'))
    } else if(id1() <0){
      indx <- sample(dim(elites.right)[1],size=5)
      temp.df <- elites.right[indx,]
      temp.df <- temp.df[order(temp.df$scores, decreasing = F),]
      Username <- paste0("<a href='https://twitter.com/",temp.df[,1],"'>",temp.df[,1],"</a>")
      renderDataTable({cbind(Username, Score = format(round(temp.df[,2], 2), nsmall = 2), Description = temp.df[,5])}, escape = FALSE, options = list(dom = 't'))

    }
  })

  tb2 <- reactive({
    if(id1() > 0){
      indx <- sample(dim(mortals.left)[1],size=5)
      temp.df <- mortals.left[indx,]
      temp.df <- temp.df[order(temp.df$scores, decreasing = T),]
      #links <- paste0("https://twitter.com/",temp.df[,1])
      Username <- paste0("<a href='https://twitter.com/",temp.df[,1],"'>",temp.df[,1],"</a>")
      describe <- lapply(temp.df[,1], function(x) user.info[user.info$screen_name == x,4])
      renderDataTable({cbind(Username, Score=format(round(temp.df[,2], 2), nsmall = 2), Description = describe)}, escape = FALSE, options = list(dom = 't'))
      #renderDataTable({cbind(Username, mortals.left[indx,2])}, escape = FALSE, options = list(dom = 'ft'))
    } else if(id1() <0){
      indx <- sample(dim(mortals.right)[1],size=5)
      temp.df <- mortals.right[indx,]
      temp.df <- temp.df[order(temp.df$scores, decreasing = F),]
      #links <- paste0("https://twitter.com/",temp.df[,1])
      Username <- paste0("<a href='https://twitter.com/",temp.df[,1],"'>",temp.df[,1],"</a>")
      describe <- lapply(temp.df[,1], function(x) user.info[user.info$screen_name == x,4])
      renderDataTable({cbind(Username, Score=format(round(temp.df[,2], 2), nsmall = 2), Description = describe)}, escape = FALSE, options = list(dom = 't'))
      #renderDataTable({cbind(Username, mortals.right[indx,2])}, escape = FALSE, options = list(dom = 'ft'))
    }
  })

  observe({
    output$myplot <- renderPlot({
      plot.estId2(id1(), input$myselect)
      })
    output$mytable <- tb2()
    output$elitestable <- tb1()
    output$values <- t1()
    load(paste0(rootp,"friends_list.RData"))
    load(paste0(rootp,"mortals_right.RData"))
    load(paste0(rootp,"mortals_left.RData"))
    load(paste0(rootp,"user_info.RData"))
  })
}
