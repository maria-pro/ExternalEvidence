
options(stringsAsFactors = FALSE)


library(rvest)
library(tidyverse)
library(skimr)
library(stringr)

#devtools::install_github("correlaid/newsanchor")
library(newsanchor)

#devtools::install_github("r-lib/usethis")
library(usethis)

#install.packages("stringr")

newsApi_key = "148eb03f26c54c8a924c1917891582dc"


#loading the file with company names
companies<-read_csv("data/listOfCompanies.csv")

searchTerms<-companies$searchTerm

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
#newsAPI<-data.frame("test")

write_csv(newsAPI, paste("data/news", format(Sys.time(), "%d-%b-%Y"),".csv", sep=""))


#----
#