
library(rvest)
library(tidyverse)

devtools::install_github("correlaid/newsanchor")
library(newsanchor)

devtools::install_github("r-lib/usethis")
library(usethis)

newsApi_key = "148eb03f26c54c8a924c1917891582dc"

results <- get_headlines(sources = "the-washington-post", api_key = newsApi_key)
results_full <- get_everything_all(query = "Abacus Property", api_key = newsApi_key, language="en")


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