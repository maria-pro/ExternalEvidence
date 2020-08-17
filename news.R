
options(stringsAsFactors = FALSE)


library(rvest)
library(tidyverse)
library(skimr)
library(stringr)

devtools::install_github("correlaid/newsanchor")
library(newsanchor)

devtools::install_github("r-lib/usethis")
library(usethis)

install.packages("stringr")

newsApi_key = "148eb03f26c54c8a924c1917891582dc"


#loading the file with company names
companies<-read_csv("data/listOfCompanies1.csv")

searchTerms<-companies$FirstTerm

dataFrame = list()

#collection for multiple companies
for (i in searchTerms){
  results_full <- get_everything_all(query =i, api_key = newsApi_key, language="en")
  
  test<-results_full$results_df%>%
    unnest()%>%
    mutate(companyName=i)
  print(test)
  dataFrame[[i]] <- data.frame(test)
}

newsAPI = data.table::rbindlist(dataFrame, fill=TRUE)

newsAPI %>% write_csv("data/news.csv")

#dataset collected using News API
newsAPI<-read_csv("data/news.csv")

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



#cleaning and descriptives

newsGoogleFull %>%
  count(companyName)




#test
results <- get_headlines(sources = "the-washington-post", api_key = newsApi_key)
results_full <- get_everything_all(query = "Abacus Property", api_key = newsApi_key, language="en")














test_metadata<-results_full$metadata%>%
  unnest() %>%
  mutate(company="Abacus group")


#search options
write_csv(newsanchor::terms_language, "data/APINewsLanguage.csv")
write_csv(newsanchor::terms_sources, "data/APINewsSources.csv")
write_csv(newsanchor::terms_country, "data/APINewsCountry.csv")
write_csv(newsanchor::terms_category, "data/APINewsCategory.csv")




APINewsCountry<-newsanchor::terms_country
APINewsCategory<-newsanchor::terms_category

# save the api_key in the .Renviron file
set_api_key(api_key = "148eb03f26c54c8a924c1917891582dc", 
            path = "~/.Renviron")

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