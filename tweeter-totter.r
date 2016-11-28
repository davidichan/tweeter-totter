install.packages("ROAuth")
library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "1LtExVSaqAUvqA2HGnAg25rJk"
consumerSecret <- "6avzckB2HK6LJ9xgrEDn3utEmLbR7GW5gB7P5bG5vgUPbZOblv"
my_oauth <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, 
                             requestURL=requestURL, accessURL=accessURL, authURL=authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

setwd("~/Documents/Big_Data/tweeter-totter/credentials")

save(my_oauth, file="my_oauth")


toInstall <- c("ggplot2", "scales", "R2WinBUGS", "devtools", "yaml", "httr", "RJSONIO")
install.packages(toInstall, repos = "http://cran.r-project.org")
library(devtools)
install_github("pablobarbera/twitter_ideology/pkg/tweetscores")

oauth_path <- "~/Documents/Big_Data/tweeter-totter/credentials"
friends <- getFriends(screen_name=user, oauth_folder=oauth_path)
