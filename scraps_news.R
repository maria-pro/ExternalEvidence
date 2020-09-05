
dataset collected using News API
#newsAPI<-read_csv(paste("data/news/"+Sys.Date()+".csv"))

#data collection using Google News 

#function to collect data
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


dataFrameGoogleNews = list()

#collection for multiple companies

for (i in searchTerms[101:213]){
  results_GoogleNews <- news(i)
  
  test<-results_GoogleNews%>%
    unnest()%>%
    mutate(companyName=i)
  
  dataFrameGoogleNews[[i]] <- data.frame(test)
}

newsGoogle4 = data.table::rbindlist(dataFrameGoogleNews, fill=TRUE)
newsGoogle4 %>% write_csv("data/newsGoogle4.csv")


newsGoogleFull<-bind_rows(newsGoogle, newsGoogle2, newsGoogle3, newsGoogle4)
newsGoogleFull %>% write_csv("data/newsGoogleFull.csv")



#



# loading the packages:
library(dplyr) # for pipes and the data_frame function
library(rvest) # webscraping
library(stringr) # to deal with strings and to clean up our data


# extracting the whole website
google <- read_html("https://news.google.com/")

# extracting the com vehicles
# we pass the nodes in html_nodes and extract the text from the last one 
# we use stringr to delete strings that are not important
vehicle_all <- google %>% 
  html_nodes("div div div main c-wiz div div div article div div div") %>% 
  html_text() %>%
  str_subset("[^more_vert]") %>%
  str_subset("[^share]") %>%
  str_subset("[^bookmark_border]")

vehicle_all[1:10] # take a look at the first ten