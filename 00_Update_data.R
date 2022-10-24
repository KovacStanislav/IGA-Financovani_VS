#---------------------------#
#        load packages      #
#---------------------------#

# required packages (with installation if needed)
list.of.packages <- c("httr","jsonlite","tidyverse","readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))


#---------------------------#
#            Login          #
#---------------------------#

cred = paste(readLines("credentials.json"), collapse="")

url_login <- "https://www.eter-project.com/api/3.0/users/login"
result_login <- POST(url_login, body = cred, content_type_json())

# translate Unicode into text
login_raw <- rawToChar(result_login$content)


# transform json into workable format for R
mylogin <- fromJSON(login_raw, flatten = TRUE)
mytoken <- mylogin$token


#---------------------------#
#         API requests      #
#---------------------------#

##  filter data within API request  ##
#  build a query

myquery <- '{
  "filter": {"BAS.REFYEAR.v": { "$in": [2013,2018]}},
  "searchTerms": []
}'

# now make the request for a filtered query
url <- "https://www.eter-project.com/api/3.0/HEIs/query"
raw_result <- POST(url, body = myquery, content_type_json(),
                   add_headers(Authorization = paste("Bearer ", mytoken)))


#---------------------------#
#       data processing     #
#---------------------------#

# check the status code ("200" for all HEIs and "201" for POST request should be the output)
raw_result$status_code

# translate unicode into text
this.raw.content <- rawToChar(raw_result$content)

# encoding to UTF-8
Encoding(this.raw.content) <- "UTF-8"

# transform JSON into workable format for R
mydata <- fromJSON(this.raw.content, flatten = TRUE)

# check if mydata is a dataframe
class(mydata)

# replace \" by nothing (run each row 2x to replace everything)
mydata$BAS.INSTNAME.v <- str_replace(mydata$BAS.INSTNAME.v, '[\"]', '')
mydata$BAS.INSTNAME.v <- str_replace(mydata$BAS.INSTNAME.v, '[\"]', '')
mydata$BAS.INSTNAMEENGL.v <- str_replace(mydata$BAS.INSTNAMEENGL.v, '[\"]', '')
mydata$BAS.INSTNAMEENGL.v <- str_replace(mydata$BAS.INSTNAMEENGL.v, '[\"]', '')


#---------------------------#
#      data manipulation    #
#---------------------------#

# drop all columns with suffix ".r" in the column name
# these columns are of technical character
mydata <- mydata[, !grepl(".r", colnames(mydata))]

# drop first 3 columns of the dataframe (metadata irrelevant for analysis)
mydata <- mydata[, -c(1:3)]

# separate data into ".code" and ".v" (== the numeric dataset)
data <- mydata[, !grepl(".v", colnames(mydata))]
data_numeric <- mydata[, !grepl(".code", colnames(mydata))]
 
# delete ".code" and ".v" from variable names
names(data) <- gsub(pattern = ".code", replacement = "", x = names(data))
names(data_numeric) <- gsub(pattern = ".v", replacement = "", x = names(data_numeric))
 
# add second "data_numeric" dataset for merging, since we need to convert
# numeric to character before merging into each other
data_numeric2 <- data_numeric
# before merging, column classes must be the same
data_numeric2 <- data_numeric2 %>%
     mutate_all(as.character)
  
# merge "data" and "data_numeric2" in "data" where NAs should vanish
data <- list(data, data_numeric2) %>%
    transpose(union(names(data), names(data_numeric2))) %>%
    map_dfc(. %>% compact %>% invoke(coalesce, .))
 
  
# add "data" to "data_numeric" also in order to add variables, where only codes
# are included
data2 <- data
  
# convert "m", "a", "x", "xc", "xr" etc. into NA
data2[data2 =="m"  | data2 =="a"  | data2 =="x" | data2 =="" | data2 =="xr" |
      data2 =="xc" | data2 =="x " | data2 =="m "] <- NA
 
data_numeric <- merge(data_numeric, data2, all.x = TRUE)


# data with only NA are of type character and thus cannot be used for
# calculations -> find and convert to numeric

colAllNA <- sapply(data_numeric, function(x) all(is.na(x)))
colnamesNA <- names(data_numeric)[colAllNA == TRUE]
data_numeric[, colnamesNA] <- apply(data_numeric[, colnamesNA], 2, function(x) as.numeric(x))



# odkiaĺ sú stĺpce RANK...? dopočítané v exceli korelace? a potrebujeme ich dopočítavať? z THE?
# vzorce pre výpočet zelených stĺpcov a oranžových na karte data a vyber
# kartu výsledky budeme aj dobudúcna napočítavať vždy na aktuálnach dátach? Alebo stačilo jednorázovo?




# dva scatter ploty z ktorých bude zrejmý rozdiel za 5 rokov medzi výškou rozpočtu a umístením v řebříčku
  # varianta 1: na x výše rozpočtu 2013 a na y zmena umístený
  # varianta 2: skore 2013 na x a na y zmena rozpočtu

