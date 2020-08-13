#twitter access

install.packages("rvest")
library("rvest")


scrape = read_html("https://twitter.com/whitehouse/")

scrape %>%html_node("title")

marketwatch_wbpg <- read_html(
  "https://www.marketwatch.com/story/bitcoin-jumps-after-credit-scare-2018-10-15"
)

marketwatch_wbpg %>%
  html_node("title") %>% #See HTML source code for data within this tag
  html_text()

test<- marketwatch_wbpg %>%
  html_nodes("p") %>% #See HTML source code for data within this tag
  html_text()


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

