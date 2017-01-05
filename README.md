# tweeter-totter

[Tweeter-totter] (http://www.tweeter-totter.xyz)

R Shiny application that lets users enter a twitter handle and get back an estimate of the twitter account’s ideological position. Based on the resulting ideology score, provides suggestions for which twitter accounts could be followed from the other side of the ideological spectrum in order to balance out the account’s twitter feed. The application is built on the [tweetscores package](https://github.com/pablobarbera/twitter_ideology) developed by Pablo Barbera. 

## Rationale
Following the US election in 2016, there was a lot of discussion about echo chambers and political polarization - people receiving news and information from other like-minded individuals only. One example often cited was that the more liberal media did not listen to what Trump supporters were saying and never really understood what they liked about the candidate. This phenomenon is exacerbated by the typical way recommendation engines work, as suggestions for users to follow are typically of the same mindset and ideological position. I noticed the same thing in my twitter feed, as it was filled with anti-Trump sentiment and not providing any insights into what people on the other end of the spectrum were thinking. This app allows users to estimate how biased their twitter feed is and suggests what accounts could be followed in order to help balance their feed.

## Query process 
Each time a query is submitted, the program searches through the list of queries already performed. If the username has never been queried before, an API call is submitted to twitter in order to obtain the list of users the account follows (friends), which is used to estimate the ideological position of the account. A second API call is also performed that downloads additional information about the queried account, including the description of the account. Both the additional account information and friends lists are stored in dataframes and written to the server such that if the same twitter account is queried again, no new API calls need to be made. 

