library(tidyverse)
library(rtweet)
# whatever name you assigned to your created app
appname <- "MariaP"

## api key (example below is not a real key)
key <- "W6G8J2SOcOpoLnidoCeM4XIIH"

## api secret (example below is not a real key)
secret <- "MHdYH2ui7cQlsXoDVk65H6gnWyqolDuyaukfJIm4Vh8Su9rnoS"

# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

#loading the file with company names
companies<-read_csv("data/listOfCompanies.csv")

searchTerms<-companies$hashtagSearch

dataFrameTwitter = list()
i<-NULL
#12
#collection for multiple companies
for (i in searchTerms){
  results_full <- search_tweets(q = i, n = 10000, lang = "en", retryonratelimit = TRUE)
  test<-results_full%>%
    mutate(companyName=i)
  print(test$companyName)
  dataFrameTwitter[[i]] <- data.frame(test)
}

TwitterAPI = data.table::rbindlist(dataFrameTwitter, fill=TRUE)

TwitterAPI<-flatten(TwitterAPI)

write_csv(TwitterAPI, paste("data/twitter", format(Sys.time(), "%d-%b-%Y"),".csv", sep=""))

write_csv(twitter_full, "twitter_full.csv")

#twitter1<-read_csv("data/twitter130-Aug-2020.csv")

#test<-TwitterAPI3%>%
#  mutate(user_id=as.double(user_id),
#         status_id=as.double(status_id),
#         reply_to_status_id=as.double(reply_to_status_id),
#         reply_to_user_id=as.double(reply_to_user_id),
#         quoted_status_id=as.double(quoted_status_id),
#         quoted_user_id=as.double(quoted_user_id),
#         retweet_status_id=as.double(retweet_status_id),
#         retweet_user_id=as.double(retweet_user_id)
#         )

#twitter_full<-full_join(test, twitter1)
#twitter_full%>%count(companyName)


