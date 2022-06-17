
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
                     RANK_num   =c(1:200,225,275,325, 450, 550, 700, 900))


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
rm(list = c("cntry_list","unlisted_cnt"))

# map region
data %<>% 
  merge(map_cntry,by = "BAS.COUNTRY") %<>% 
  merge(map_rank, by = "RANK")

# change variable types for numerical
for (col in colnames(data)){
  if (sum(is.na(as.numeric(data[,col])),na.rm=T) < (n/2) ) {
    data[,col] = as.numeric(data[,col])
  }
}

numerics = unlist(lapply(data, is.numeric), use.names = F)


i = 0
# visualize all variables by region
for(col in numerics){
  i= i+1
  if (numerics[i]) {
    boxplot(data[,i], main = colnames(data)[i])
  }
}

sum(numerics)

GGally::ggpairs(data=)


rm(list = c("map_cntry","map_rank","col"))


summary(lm(RANK_num ~ EXP.CURRPERSON.EURO,data))

plot(y=data$RANK_num,x=data$EXP.CURRPERSON.EURO)

# zistiť alternatívu k testovaniu hypotéz pre vyčerpávajúce šetrenie
# sledovať nielen pre viac premenných, ale aj len pre jednu financie vs ranking (zatiaľ nie, možno v ďalšom kroku)
# ranking vs "HDP per capita" alebo podobne ale očistiť rozpočtu (total current revenue/expenditure)
# závislosť významná s výdajmi na research
# 

# použiť ako vysvetľujúcu premennú EXP.CURRTOTAL.EURO

ggplot(data, aes(x = EXP.CURRPERSON.EURO, y = RANK_num, col = REGION)) +
  geom_point()
ggplot(data %>% filter(REGION != "WE"), aes(x = EXP.CURRPERSON.EURO, y = RANK_num, col = REGION)) +
  geom_point()


data %>% 
  filter(REGION == "MEE", RANK_num < 500) %>% 
  select(BAS.COUNTRY,BAS.ETERIDYEAR)



unique(data_prem$Variable)
# zoznam numerických dobrých premenných 




GGally::ggpairs(data %>% select(RANK.OVERALL.TRUE, 
                                data_prem$Variable[seq(0,length(data_prem$Variable),10)]))

# doplniť HDP a sledovať závislosť



data$STUD.ISCED5_7FOE08_SHARE

summary(lm(RANK.OVERALL.TRUE~
            EXP.CURRNONPERSON.EURO + 
             PERS.TOTACAHC+ 
            IND.RES.PHDINTENSITY+
             STUD.PER.STAFF, 
           data = data))



plot(data$STUD.TOTALISCED5_7, data$PERS.TOTACAHC)
