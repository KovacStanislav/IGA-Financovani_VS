
# Data definition --------------------------------------------------------------

# spustenie 01_Load_data 
source("01_Load_data.R")

data_v %>% 
  select(BAS.INSTNAMEENGL,
         BAS.COUNTRY,
         REGION,
         EXP.CURRPERSON.EURO,
         RANK.OVERALL.TRUE,
         STUD.TOTALISCED5_7) %>% 
  na.omit

karlovka = data_v %>% filter(BAS.INSTNAMEENGL == "Charles University")
komenskeho = data_v %>% filter(BAS.INSTNAMEENGL == "Comenius University in Bratislava")


k_exp_data = data_v %>% filter(between(data$EXP.CURRPERSON.EURO,
                                     karlovka$EXP.CURRPERSON.EURO*.9,
                                     karlovka$EXP.CURRPERSON.EURO*1.1)) %>% 
                      arrange(STUD.TOTALISCED5_7) %>% 
                      mutate(k_flag = BAS.INSTNAMEENGL == "Charles University")


ko_exp_data = data_v %>% filter(between(data$EXP.CURRPERSON.EURO,
                                     komenskeho$EXP.CURRPERSON.EURO*.9,
                                     komenskeho$EXP.CURRPERSON.EURO*1.1)) %>% 
                        arrange(STUD.TOTALISCED5_7) %>% 
                        mutate(k_flag = BAS.INSTNAMEENGL=="Comenius University in Bratislava")

# Data vizualization ----------------------------------------------------------



ggplot(data = k_exp_data, aes(x=STUD.TOTALISCED5_7,y=RANK.OVERALL.TRUE, color = REGION))+
  geom_point()

ggplot(data = ko_exp_data, aes(x=STUD.TOTALISCED5_7,y=RANK.OVERALL.TRUE, color = REGION))+
  geom_point()


ggplot(data = k_exp_data, aes(x=BAS.INSTNAMEENGL,y=STUD.TOTALISCED5_7))+
  geom_point() # TODO zoradiť






