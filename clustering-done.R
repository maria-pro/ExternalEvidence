library(tidyverse)
library(data.table)
install.packages("readxl")
library(readxl)

test<-read_excel("data/companyData1.xlsm")

xl_data <- "data/companyData1.xlsm"

# Before reading data, we will return the names of the sheets for later use:
tab_names <- excel_sheets(path = xl_data)

list_all <- tab_names %>%
  map(function(x) read_excel(path = xl_data, sheet = x))

str(list_all)

df <- data.table::rbindlist(list_all, fill=TRUE)

df$SECD<-NULL

df1<-df %>% pivot_longer(col=-Name, names_to="Year", values_to="Value")

df2<-df1 %>%filter(Name=="#ERROR")

df2 %>%
  
  
SalesInRec=  AccRec/sales
GrossMargin=(Sales-COGS)/Sales

AssetQuality=1-(CAssets+PPE)/TAssets
SalesRatio=Sales/TAssets
DeprRate=Depreciation/(Depreciation+PPE)
SGARatio=SGA/Sales
LeverageRatio=TDebt/TAssets
AccrRatio=TAccr/TAssets
RSSTAccr=(ChangeNonCashWC+ChangeNonCurOperAssets+ChangeNetFinAssets)AverageTAssets
ChangeRecRatio=ChangeRec/AverageTAssets
ChangeInvRatio=ChangeInv/AverageTAssets
SoftAssets=(TAssets-PPE-Cash)/TAssets
#ChangeCashSales=percentage change in cash receits
#ChangeRoA=Earnings/AverageTAssets-previous year Earnings/AverageTAssets=RoA- RoA
Issue=1,0 new securities








#------


test<-read_csv("data/testData/WC01101.csv")



test_clean<-test%>%filter(Name!="#ERROR")

test_na<-test%>%filter(Name=="#ERROR")



excel_sheets(readxl_example("data/companyData.xlsm"))
#> [1] "iris"     "mtcars"   "chickwts" "quakes"  
excel_sheets(readxl_example("datasets.xls"))
#> [1] "iris"     "mtcars"   "chickwts" "quakes"  

# To load all sheets in a workbook, use lapply()
path <- readxl_example("datasets.xls")
lapply(excel_sheets(path), read_excel, path = path)

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