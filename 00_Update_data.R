#---------------------------#
#        load packages      #
#---------------------------#

# required packages (with installation if needed)
list.of.packages <- c("httr","jsonlite","tidyverse","readr", "readxl", "dplyr", 
                      "data.table", "purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()
                                   [,"Package"])]
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

# check the status code ("200" for all HEIs and "201" for POST request should 
#be the output)
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
names(data_numeric) <- gsub(pattern = ".v", replacement = "", 
                            x = names(data_numeric))



# add second "data_numeric" dataset for merging, since we need to convert
# numeric to character before merging into each other
data_numeric2 <- data_numeric
# before merging, column classes must be the same
data_numeric2 <- data_numeric2 %>%
  mutate_all(as.character)

# merge "data" and "data_numeric2" in "data" where NAs should vanish

data <- merge(data, data_numeric2, all.y = TRUE)

# read data from an Excel file to get variable names
correlation <- read_excel("ETER_THE_2018_ext - korelace.xlsx", sheet = 1)
correlation <- correlation[-1,]


# join data frames
df <- correlation %>% semi_join(data, by = "BAS.REFYEAR")
data2 <- df
data_numeric <- merge(data_numeric, data2, all.x = TRUE)

# data with only NA are of type character and thus cannot be used for
# calculations -> find and convert to numeric
colAllNA <- sapply(data_numeric, function(x) all(is.na(x)))
colnamesNA <- names(data_numeric)[colAllNA == TRUE]
data_numeric[, colnamesNA] <- apply(data_numeric[, colnamesNA], 2, 
                                    function(x) as.numeric(x))

# manipulating data to get a proper looking table
df <- data_numeric[data_numeric$BAS.REFYEAR == '2018',]
instname <- as.vector(correlation$BAS.INSTNAME)
instname <- df[df$BAS.INSTNAME %in% instname, ]
column_names <- as.vector(intersect(colnames(correlation), colnames(instname)))
instname <- instname %>% dplyr::select(all_of(column_names))
data_numeric_fin <- merge(correlation, instname, by = column_names, all.y = TRUE)
correlation2 <- correlation %>% dplyr::select(c(1:19))
data_numeric_fin <- merge(correlation2, data_numeric_fin, by = "BAS.INSTNAME",
                          all = TRUE)
data_numeric_fin[data_numeric_fin =="m"  | data_numeric_fin =="a"  | 
                   data_numeric_fin =="x" | data_numeric_fin =="" | 
                   data_numeric_fin =="xr" |
                   data_numeric_fin =="xc" | data_numeric_fin =="x " | 
                   data_numeric_fin =="m "] <- NA
data_numeric_fin <- data_numeric_fin[, 
                                     -c(grep("NAME.ETER.y", 
                                             colnames(data_numeric_fin)
                                     ):grep("BAS.WHEDID.y", 
                                            colnames(data_numeric_fin)))]
data_numeric_fin <- data_numeric_fin %>% relocate(BAS.INSTNAME,
                                                  .after = BAS.WHEDID.x)
column_names <- as.vector(colnames(data_numeric_fin[,1:grep("BAS.WHEDID.x",
                                                            colnames(data_numeric_fin))]))
column_names2 <- as.vector(colnames(correlation2[,-grep("BAS.INSTNAME",
                                                        colnames(correlation2))]))
setnames(data_numeric_fin, old = column_names, new = column_names2)

# replace NA with values (these columns were missing in the main data, but are 
# a part of an excel table)
data_numeric_fin$STUD.ISCED5_7FOE00_SHARE <- data_numeric_fin$STUD.ISCED5_7FOE00/
  data_numeric_fin$STUD.TOTALISCED5_7
data_numeric_fin$STUD.ISCED5_7FOE01_SHARE <- data_numeric_fin$STUD.ISCED5_7FOE01/
  data_numeric_fin$STUD.TOTALISCED5_7
data_numeric_fin$STUD.ISCED5_7FOE02_SHARE <- data_numeric_fin$STUD.ISCED5_7FOE02/
  data_numeric_fin$STUD.TOTALISCED5_7
data_numeric_fin$STUD.ISCED5_7FOE03_SHARE <- data_numeric_fin$STUD.ISCED5_7FOE03/
  data_numeric_fin$STUD.TOTALISCED5_7
data_numeric_fin$STUD.ISCED5_7FOE04_SHARE <- data_numeric_fin$STUD.ISCED5_7FOE04/
  data_numeric_fin$STUD.TOTALISCED5_7
data_numeric_fin$STUD.ISCED5_7FOE05_SHARE <- data_numeric_fin$STUD.ISCED5_7FOE05/
  data_numeric_fin$STUD.TOTALISCED5_7
data_numeric_fin$STUD.ISCED5_7FOE06_SHARE <- data_numeric_fin$STUD.ISCED5_7FOE06/
  data_numeric_fin$STUD.TOTALISCED5_7
data_numeric_fin$STUD.ISCED5_7FOE07_SHARE <- data_numeric_fin$STUD.ISCED5_7FOE07/
  data_numeric_fin$STUD.TOTALISCED5_7
data_numeric_fin$STUD.ISCED5_7FOE08_SHARE <- data_numeric_fin$STUD.ISCED5_7FOE08/
  data_numeric_fin$STUD.TOTALISCED5_7
data_numeric_fin$STUD.ISCED5_7FOE09_SHARE <- data_numeric_fin$STUD.ISCED5_7FOE09/
  data_numeric_fin$STUD.TOTALISCED5_7
data_numeric_fin$STUD.ISCED5_7FOE10_SHARE <- data_numeric_fin$STUD.ISCED5_7FOE10/
  data_numeric_fin$STUD.TOTALISCED5_7

data_numeric_fin$PERS.ACAHCFOE00_SHARE <- data_numeric_fin$PERS.ACAHCFOE00/
  data_numeric_fin$PERS.TOTACAHC
data_numeric_fin$PERS.ACAHCFOE01_SHARE <- data_numeric_fin$PERS.ACAHCFOE01/
  data_numeric_fin$PERS.TOTACAHC
data_numeric_fin$PERS.ACAHCFOE02_SHARE <- data_numeric_fin$PERS.ACAHCFOE02/
  data_numeric_fin$PERS.TOTACAHC
data_numeric_fin$PERS.ACAHCFOE03_SHARE <- data_numeric_fin$PERS.ACAHCFOE03/
  data_numeric_fin$PERS.TOTACAHC
data_numeric_fin$PERS.ACAHCFOE04_SHARE <- data_numeric_fin$PERS.ACAHCFOE04/
  data_numeric_fin$PERS.TOTACAHC
data_numeric_fin$PERS.ACAHCFOE05_SHARE <- data_numeric_fin$PERS.ACAHCFOE05/
  data_numeric_fin$PERS.TOTACAHC
data_numeric_fin$PERS.ACAHCFOE06_SHARE <- data_numeric_fin$PERS.ACAHCFOE06/
  data_numeric_fin$PERS.TOTACAHC
data_numeric_fin$PERS.ACAHCFOE07_SHARE <- data_numeric_fin$PERS.ACAHCFOE07/
  data_numeric_fin$PERS.TOTACAHC
data_numeric_fin$PERS.ACAHCFOE08_SHARE <- data_numeric_fin$PERS.ACAHCFOE08/
  data_numeric_fin$PERS.TOTACAHC
data_numeric_fin$PERS.ACAHCFOE09_SHARE <- data_numeric_fin$PERS.ACAHCFOE09/
  data_numeric_fin$PERS.TOTACAHC
data_numeric_fin$PERS.ACAHCFOE10_SHARE <- data_numeric_fin$PERS.ACAHCFOE10/
  data_numeric_fin$PERS.TOTACAHC
data_numeric_fin$STUD.PER.STAFF <- data_numeric_fin$STUD.TOTALISCED5_7/
  data_numeric_fin$PERS.TOTALFTE
data_numeric_fin$EXP.PER.STUDENT <- data_numeric_fin$EXP.CURRTOTAL.EURO/
  data_numeric_fin$STUD.TOTALISCED5_7

# graphs
ranking <- read_excel("rankings results 2013 a 2018.xlsx")
ranking <- ranking[-1,]
data_numeric_fin3 <- data_numeric_fin %>% dplyr::select(c("NAME.ETER",
                                                          "BAS.ETER.ID"))
ranking_fin <- merge(data_numeric_fin3, ranking, by = "NAME.ETER")
budget <- read_excel("ETER_BUDGET.xlsx")
budget <- budget[-1,-c(2,3)]
budget[budget =="information missing" | budget == "included in another row" | 
         budget == "breakdown not available, but included in total" | 
         budget == "not applicable" | budget == "confidential"]<- NA
budget2013 <- budget[budget$BAS.REFYEAR == '2013',]
colnames(ranking_fin)[2] <- 'BAS.ETERID'
budget2013 <- budget2013[,-2]
ranking_fin2 <- merge(ranking_fin, budget2013, by = "BAS.ETERID")
ranking_fin2$REV.CORETOTAL.EURO <- as.numeric(ranking_fin2$REV.CORETOTAL.EURO)
change_2013/2018 <- as.numeric(ranking_fin2$RANK.2018.TRUERANK)-
  as.numeric(ranking_fin2$RANK.2013.TRUERANK)
ranking_fin2$change_2013/2018 <- change_2013/2018
ranking_fin2$REV.CORETOTAL.EURO <- 
  format(round(as.numeric(ranking_fin2$REV.CORETOTAL.EURO) / 1e6, 1),
         trim = TRUE)
ranking_fin2$REV.CORETOTAL.EURO <- as.numeric(ranking_fin2$REV.CORETOTAL.EURO)


rank_budget <- ggplot(ranking_fin2, aes(x = REV.CORETOTAL.EURO, y = change_2013/2018)) +
  geom_point(size = 2, shape = 21, aes(fill = change_2013/2018 > 0),
             show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  xlab("Total core budget") + ylab("Rank change")

rank_budget # weak negative correlation, variables hardly affect each other, 
# rather linear association. the size of the total budget in 2013 had no impact 
# on the rating change in 2018

cor(as.numeric(ranking_fin2$REV.CORETOTAL.EURO), as.numeric(ranking_fin2$change_2013/2018),
    method = "pearson", use = "complete.obs")

budget2018 <- budget[budget$BAS.REFYEAR == '2018',]
budget2018 <- budget2018[,-2]
ranking_fin2 <- merge(ranking_fin, budget2013, by = "BAS.ETERID")
ranking_fin2 <- merge(ranking_fin2, budget2018, by = "BAS.ETERID")
change_2013/20182 <- as.numeric(ranking_fin2$REV.CORETOTAL.EURO.y)-
  as.numeric(ranking_fin2$REV.CORETOTAL.EURO.x)
change_2013/20182 <- as.numeric(change_2013/20182)
change_2013/20182 <- format(round(as.numeric(change_2013/20182) / 1e6, 1), trim = TRUE)
ranking_fin2$change_2013/20182 <- as.numeric(change_2013/20182)
ranking_fin2$RANK.2013.TRUERANK <- as.numeric(ranking_fin2$RANK.2013.TRUERANK)

budget_rank <- ggplot(ranking_fin2, aes(x = RANK.2013.TRUERANK, y = change_2013/20182)) +
  geom_point(size = 2, shape = 21, aes(fill = change_2013/20182 > 0),
             show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  xlab("Rank 2013") + ylab("Total core budget change")

budget_rank # weak negative correlation, variables hardly affect each other,
# non-linear association. the low rating of an educational institution does 
#not lead to an increase in the overall budget.


# creating a correlation table based on the excel file
correlation_excel <- read_excel("ETER_THE_2018_ext - korelace.xlsx", 
                                sheet = 3)

data_for_correlation <- data_numeric_fin %>%
  dplyr::select(contains(correlation_excel$Variable))

column_names4 <- colnames(correlation_excel[,grep("RANK.OVERALL.TRUE", 
                                                  colnames(correlation_excel)):
                                              grep("RANK.INTERNATIONAL", 
                                                   colnames(correlation_excel))])

for (i in 1:length(column_names4)) {
  assign(column_names4[i], c())
}
data_for_correlation3 <- c()
for (i in 1:ncol(data_for_correlation)) {
  if ((all(is.na(as.numeric(data_for_correlation[,i]))))==FALSE) {
    RANK.OVERALL.TRUE <- c(RANK.OVERALL.TRUE, 
                           cor(as.numeric(data_numeric_fin$RANK.OVERALL.TRUE), 
                               as.numeric(data_for_correlation[,i]),  
                               method = "pearson", use = "complete.obs"))
    RANK.TEACHING <- c(RANK.TEACHING, 
                       cor(as.numeric(data_numeric_fin$RANK.TEACHING), 
                           as.numeric(data_for_correlation[,i]),  
                           method = "pearson", use = "complete.obs"))
    RANK.CITATIONS <- c(RANK.CITATIONS, 
                        cor(as.numeric(data_numeric_fin$RANK.CITATIONS), 
                            as.numeric(data_for_correlation[,i]),  
                            method = "pearson", use = "complete.obs"))
    RANK.INDUSTRYINC <- c(RANK.INDUSTRYINC, 
                          cor(as.numeric(data_numeric_fin$RANK.INDUSTRYINC), 
                              as.numeric(data_for_correlation[,i]),  
                              method = "pearson", use = "complete.obs"))
    RANK.INTERNATIONAL <- c(RANK.INTERNATIONAL, 
                            cor(as.numeric(data_numeric_fin$RANK.INTERNATIONAL),
                                as.numeric(data_for_correlation[,i]),  
                                method = "pearson",
                                use = "complete.obs"))
    RANK.RESEARCH <- c(RANK.RESEARCH, 
                       cor(as.numeric(data_numeric_fin$RANK.RESEARCH), 
                           as.numeric(data_for_correlation[,i]),  
                           method = "pearson", 
                           use = "complete.obs"))
    
  }
}
for (i in 1:ncol(data_for_correlation)) {
  if ((all(is.na(as.numeric(data_for_correlation[,i]))))==FALSE) {
    data_for_correlation3 <- c(data_for_correlation3, 
                               as.vector(colnames(data_for_correlation[i])))
  }
}

correlation_excel <- correlation_excel[correlation_excel$Variable %in% 
                                         data_for_correlation3, ]

data_for_correlation2 <- data.frame(Variable = data_for_correlation3,
                                    RANK.OVERALL.TRUE = RANK.OVERALL.TRUE,
                                    RANK.TEACHING = RANK.TEACHING,
                                    RANK.CITATIONS = RANK.CITATIONS,
                                    RANK.INDUSTRYINC = RANK.INDUSTRYINC,
                                    RANK.INTERNATIONAL = RANK.INTERNATIONAL,
                                    RANK.RESEARCH = RANK.RESEARCH,
                                    Blaa = NA,
                                    Bleee = NA,
                                    n = NA
)
helping_table <- read_excel("ETER_THE_2018_ext - korelace.xlsx", sheet = 3)
helping_table <- helping_table[,-c(1,3)]
helping_table <- helping_table[helping_table$Variable %in% data_for_correlation3, ]


column_names5 <- as.vector(colnames(helping_table))
column_names6 <- as.vector(match(column_names5, colnames(data_for_correlation2)))
data_for_correlation2 <- data_for_correlation2[, column_names6]
correlation_excel <- correlation_excel[ ,-c(grep("RANK.OVERALL.TRUE", 
                                                 colnames(correlation_excel)):
                                              grep("^n$", colnames(correlation_excel)))]

correlation <- merge(correlation_excel, data_for_correlation2, by = "Variable", 
                     all=TRUE)
correlation <- correlation %>% relocate(Variable, .after = Tema)










