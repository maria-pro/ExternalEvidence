install.packages("rtweet")
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


HKTweets <- search_tweets(q = "HongKongProtest", n = 10000, lang = "en")
HKTweets2 <- search_tweets(q = "HongKongProtests", n = 10000, lang = "en")
HKTweets3 <- search_tweets(q = "antiextraditionlaw", n = 10000, lang = "en")
HKTweets4 <- search_tweets(q = "notochinaextradition", n = 10000, lang = "en")
HKTweets5 <- search_tweets(q = "hongkongers", n = 10000, lang = "en")
HKTweets6 <- search_tweets(q = "extradictionbill", n = 10000, lang = "en")

save_as_csv(HKTweets, "HKTweets.csv")
save_as_csv(HKTweets2, "HKTweets2.csv")
save_as_csv(HKTweets3, "HKTweets3.csv")
save_as_csv(HKTweets4, "HKTweets4.csv")
save_as_csv(HKTweets5, "HKTweets5.csv")
save_as_csv(HKTweets6, "HKTweets6.csv")


GlutenTweets <- search_tweets(q = "glutenfree", n = 10000, lang = "en")
GlutenTweets2 <- search_tweets(q = "healthysnacks", n = 10000, lang = "en")
GlutenTweets3 <- search_tweets(q = "coeliacs", n = 10000, lang = "en")
save_as_csv(GlutenTweets, "GlutenTweets.csv")
save_as_csv(GlutenTweets2, "GlutenTweets2.csv")
save_as_csv(GlutenTweets3, "GlutenTweets3.csv")


CocoTweets <- search_tweets(q = "chocolate", n = 10000, lang = "en")
CocoTweets2 <- search_tweets(q = "darkchocolate", n = 10000, lang = "en")
CocoTweets3 <- search_tweets(q = "milkchocolate", n = 10000, lang = "en")
CocoTweets4 <- search_tweets(q = "whitechocolate", n = 10000, lang = "en")
save_as_csv(CocoTweets, "CocoTweets.csv")
save_as_csv(CocoTweets2, "CocoTweets2.csv")
save_as_csv(CocoTweets3, "CocoTweets3.csv")
save_as_csv(CocoTweets4, "CocoTweets4.csv")


CocoTweetsFr <- search_tweets(q = "#café", n = 10000)
CocoTweetsFr2 <- search_tweets(q = "#caffèlatte", n = 10000, lang = "fr")
CocoTweetsFr3 <- search_tweets(q = "chocolataulait", n = 10000, lang = "fr")
CocoTweetsFr4 <- search_tweets(q = "chocolatblanc", n = 10000, lang = "fr")
save_as_csv(CocoTweetsFr, "CocoTweetsFr.csv")
save_as_csv(CocoTweetsFr2, "CocoTweetsFr2.csv")
save_as_csv(CocoTweetsFr3, "CocoTweetsFr3.csv")
save_as_csv(CocoTweetsFr4, "CocoTweetsFr4.csv")


CTweets <- search_tweets(q = "#capuchino", n = 100000, lang = "es", retryonratelimit = TRUE)
CTweets2 <- search_tweets(q = "#expresado", n = 100000, lang = "es", retryonratelimit = TRUE)
CTweets3 <- search_tweets(q = "#chocolatecaliente", n = 100000, lang = "es", retryonratelimit = TRUE)
CTweets4 <- search_tweets(q = "#leche", n = 100000, lang = "es", retryonratelimit = TRUE)
CTweets5 <- search_tweets(q = "#latte", n = 100000, lang = "es", retryonratelimit = TRUE)
CTweets6 <- search_tweets(q = "#manchado", n = 100000, lang = "es", retryonratelimit = TRUE)
CTweets7 <- search_tweets(q = "#cafefrio", n = 100000, lang = "es", retryonratelimit = TRUE)
CTweets8 <- search_tweets(q = "#café", n = 100000, lang = "es", retryonratelimit = TRUE)
CTweets9 <- search_tweets(q = "#cappuccino", n = 100000, lang = "en", retryonratelimit = TRUE)

save_as_csv(CTweets, "CTweets.csv")
save_as_csv(CTweets2, "CTweets2.csv")
save_as_csv(CTweets3, "CTweets3.csv")
save_as_csv(CTweets4, "CTweets4.csv")
save_as_csv(CTweets5, "CTweets5.csv")
save_as_csv(CTweets6, "CTweets6.csv")
save_as_csv(CTweets7, "CTweets7.csv")
save_as_csv(CTweets8, "CTweets8.csv")
save_as_csv(CTweets9, "CTweets9.csv")


test<-Reduce(function(x, y) merge(x, y, all=TRUE), list(CTweets, CTweets2, CTweets3, CTweets4, CTweets5, CTweets6, CTweets7, CTweets8))



save_as_csv(HKTweets, "HKTweets.csv")
save_as_csv(HKTweets2, "HKTweets2.csv")
save_as_csv(HKTweets3, "HKTweets3.csv")
save_as_csv(HKTweets4, "HKTweets4.csv")
save_as_csv(HKTweets5, "HKTweets5.csv")
save_as_csv(HKTweets6, "HKTweets6.csv")
save_as_csv(GlutenTweets, "GlutenTweets.csv")
save_as_csv(GlutenTweets2, "GlutenTweets2.csv")
save_as_csv(CocoTweets, "CocoTweets.csv")
save_as_csv(CocoTweetsFr, "CocoTweetsFr.csv")

