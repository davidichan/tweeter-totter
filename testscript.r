#testscript


r <- "blah"

blah <- list(hspter=friends_hspter)
blah <- append(blah, list(david=friends_hspter))

addfriends <- function(usrname, fr_list=friends.list){
  fr <- getFriends(screen_name=usrname, oauth_folder="~/Documents/Big_Data/t-t/credentials")
  fr_list <- append(fr_list, list(tempname = fr))
  names(fr_list)[names(fr_list) == "tempname"] <- usrname
  return(fr_list)
}

name_list <- c("FoxNews", "hspter")
for(usrname in name_list){
  if(usrname %in% names(blah2)){
    #if name is in list, then use friends list in here
    estimateIdeology(usrname, blah2[[usrname]], method="MLE")
  } else {
    #if name is not in list, then create a new entry in the list
    blah2 <- addfriends(usrname, blah2)
    estimateIdeology(usrname, blah2[[usrname]], method="MLE")
  }
}

