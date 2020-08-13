
library(rvest)
library(tidyverse)


news <- function(term) {
  
  html_dat <- read_html(paste0("https://news.google.com/search?q=",term,"&hl=en-US&gl=US&ceid=US%3Aen"))
  
  dat <- data.frame(Link = html_dat %>%
                      html_nodes('.VDXfz') %>% 
                      html_attr('href')) %>% 
    mutate(Link = gsub("./articles/","https://news.google.com/articles/",Link))
  
  news_dat <- data.frame(
    Title = html_dat %>%
      html_nodes('.DY5T1d') %>% 
      html_text(),
    Link = dat$Link,
    Description =  html_dat %>%
      html_nodes('.Rai5ob') %>% 
      html_text()
  )
  
  return(news_dat)
}

test<-news("Woolworths")