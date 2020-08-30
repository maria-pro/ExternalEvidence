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

#collection for multiple companies
for (i in searchTerms){
  results_full <- search_tweets(q = i, n = 10000, lang = "en", retryonratelimit = TRUE)
  test<-results_full%>%
    mutate(companyName=i)
  print(test$companyName)
  dataFrameTwitter[[i]] <- data.frame(test)
}

TwitterAPI = data.table::rbindlist(dataFrameTwitter, fill=TRUE)
#newsAPI<-data.frame("test")

write_csv(newsAPI, paste("data/twitter", format(Sys.time(), "%d-%b-%Y"),".csv", sep=""))



marketwatch_bitcoin_articles <- read_html(
  "https://www.marketwatch.com/search?q=bitcoin&m=Keyword&rpp=15&mp=0&bd=false&rs=false"
)


datetime <- marketwatch_bitcoin_articles %>%
  html_nodes("div.deemphasized span") %>% #See HTML source code for data within this tag
  html_text()

twitter<-read_html("https://mobile.twitter.com/hashtag/cats")

twitter %>% 
  html_nodes("tweet") %>%
  html_text




scrape.count <- scrape %>% 
  html_nodes(".js-nav .ProfileNav-value") %>%
  html_text() %>%
  as.character()
scrape.count = as.numeric(paste(substr(scrape.count,1,1),substr(scrape.count,3,5),sep=""))


#news access

