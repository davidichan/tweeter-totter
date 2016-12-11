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



# pl1 <- reactive({
#    Theta = (-1) * id1()
#    #ggplot(data=refpoints, aes(X1, score, label=X.FoxNews.)) + geom_point() + twt_labs + geom_segment(aes_string(x = -0.9, xend = 0.9, y = 0.2, yend = Theta), alpha = 0.2, color="blue4", size=4, lineend = "round") 
#    t_param <- theme(axis.text.y = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(), panel.background = element_blank())
#    v_line <- geom_segment(aes(x = 0, xend = 0, y = 0.8, yend = 2.3), size=10, lineend = "round", colour="chocolate")
#    main_line <- geom_segment(aes(x = -0.9, xend = 0.9, y = 2.5 + -1 * Theta, yend = 2.5 + 1 * Theta ), alpha = 0.2, color="blue4", size=6, lineend = "round") 
#    twt_labs <- geom_label(hjust = 0, nudge_x = 0.05)
#    ggplot(data=refpoints, aes(X1, score, label=X.FoxNews.)) + geom_point() + twt_labs + main_line + xlim(-1.5,1.5) + ylim(0, 5) + t_param + v_line + xlab("Ideology") + ylab("")
#  })
