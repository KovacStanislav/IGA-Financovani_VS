
# required packages (with installation if needed)
list.of.packages <- c("ggplot2","readxl","tidyverse","magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))

# Map table for countries to region
map_cntry=data.frame(BAS.COUNTRY=c("UK","FR","DE","LU","BE","AT","CH","IE","NL",
                                   "NO","FI","DK","SE","IS",
                                   "SK","PL","HU","CZ","EE","LV","LT","RO","SI",
                                   "ES","PT","HR","GR","IT",
                                   "TR"),
                     REGION   =c(rep("WE", 9),
                                 rep("NE", 5),
                                 rep("MEE",9),
                                 rep("SE", 5),
                                 rep("TR", 1)))

map_rank =data.frame(RANK=c(1:200,
                            "201-250","251-300","301-350",
                            "401-500", "501-600",
                            "601-800", "801-1000"),
                     RANK_num =c(1:200,225,275,325, 450, 550, 700, 900))

map_gdp= read_xlsx(path="GDP_per_capita.xlsx", 
                   sheet="Sheet 1", range = "A10:B55") %>% 
  tibble %>% merge(read.csv("Dim_Countries_EuroStat.csv",quote = '"'),
                   by.x= "GEO (Labels)", by.y = "country_name")

colnames(map_gdp) = c("GEO","GDP_pc","BAS.COUNTRY" )

data =  read_xlsx(path = "ETER_THE_2018_ext - korelace.xlsx", sheet="data") %>% tibble # load data
data_prem =  read_xlsx(path = "ETER_THE_2018_ext - korelace.xlsx", sheet="vysledky") %>% tibble # load data

# first row with column description
col_description = data %>% filter(row_number() == 1) 

# remove column description
data = data %>% filter(!row_number() == 1)
n = nrow(data)

# find countries not listed in our region mapping table 
cntry_list = data %>% select(BAS.COUNTRY) %>% distinct() %>% pull() 
unlisted_cnt = length(cntry_list[!cntry_list %in% map_cntry$BAS.COUNTRY])
if (unlisted_cnt>0) message("There are countries not listed within region")

# map region
data %<>% 
  merge(map_cntry,by = "BAS.COUNTRY") %<>% 
  merge(map_gdp, by = "BAS.COUNTRY") %<>% 
  merge(map_rank, by = "RANK")

rm(list = c("cntry_list","unlisted_cnt","map_cntry","map_gdp","map_rank"))



# change variable types for numerical
for (col in colnames(data)){
  if (sum(is.na(as.numeric(data[,col])),na.rm=T) < (n/2) ) {
    data[,col] = as.numeric(data[,col])
  }
}
