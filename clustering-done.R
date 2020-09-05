library(tidyverse)
library(data.table)
install.packages("readxl")
library(readxl)

test<-read_excel("data/companyData.xlsm")


test<-read_csv("data/testData/WC01101.csv")
test_clean<-test%>%filter(Name!="#ERROR")

test_na<-test%>%filter(Name=="#ERROR")

#task: check missing
#task: remove name of accounts from Name - also IDs for company names to ADD




  
  
  
tbl_fread <- list.files(path = "./data/testData", full.names = TRUE, recursive = TRUE)%>% 
  map_df(~read.csv(.))

tbl_fread%>%count(Name)

tbl_fread %>% map(c(2,2))
  
tbl_fread[[1]]
class(tbl_fread)

datastream<-data.table::rbindlist(tbl_fread, fill=TRUE)


datastream<-(flatten(tbl_fread))


list.files(path="./data/testData", pattern = "*.csv") %>% 
  map_df(~fread(.))




test_data <- lapply(tbl_fread, read.csv,
                    sep ="\t",
                    fill = TRUE,
                    quote='', 
                    header = FALSE )