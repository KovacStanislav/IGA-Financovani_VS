
# spustenie 01_Load_data 
source("01_Load_data.R")

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
# počet numerických nie moc chýbajúcich premenných 




GGally::ggpairs(data %>% select(RANK.OVERALL.TRUE, 
                                data_prem$Variable[seq(0,length(data_prem$Variable),10)]))

# doplniť HDP a sledovať závislosť

set1 = data_prem %>% filter(Tema == "FINANCE", abs(RANK.OVERALL.TRUE)>.25) %>% .$Variable
set2 = data_prem %>% filter(Tema == "STAFF", abs(RANK.OVERALL.TRUE)>.25) %>% .$Variable
set3 = data_prem %>% filter(Tema == "STUDENTI A ABSOLVENTI", abs(RANK.OVERALL.TRUE)>.25) %>% .$Variable
set4 = data_prem %>% filter(Tema == "OBORY - STUDENTI A ABSOLVENTI", abs(RANK.OVERALL.TRUE)>.25) %>% .$Variable

generate_model_def = function(i,n,dfc){
  if (n == 4) md = paste0("paste0('mylm = lm(RANK.OVERALL.TRUE~',dfc[",i,
                         ",1],'+',dfc[",i,",2],'+',dfc[",i,",3],'+',dfc[",i,
                         ",4],',data = data)')") else md = 
                 paste0("paste0('mylm = lm(RANK.OVERALL.TRUE~',dfc[",i,
                        ",1],'+',dfc[",i,",2],'+',dfc[",i,
                        ",3],',data = data)')")
}


df_comb = expand.grid(set1,set2,set3,set4)
dfc4 = cbind(as.character(df_comb$Var1),
            as.character(df_comb$Var2),
            as.character(df_comb$Var3),
            as.character(df_comb$Var4))
dfc3 = dfc4[,1:3]

calc_rsq = function(dfc){
  results = c()
  for (i in 1:nrow(df_comb)){
    text_to_eval = generate_model_def(i,ncol(dfc),dfc)
    eval(parse(text=eval(parse(text=text_to_eval))))
    results <- c(results,summary(mylm)$r.squared)
  }
  return(results)
}

r4 = calc_rsq(dfc4)

best_result = max(r4) 
which_is_best = which.max(r4)
dfc4[which_is_best,]

dfc = dfc4

eval(parse(text=eval(parse(text=generate_model_def(which_is_best,4,dfc4)))))
summary(mylm)

r3 = calc_rsq(dfc3)

best_result = max(r3) 
which_is_best = which.max(r3)
dfc3[which_is_best,]

dfc = dfc3

eval(parse(text=eval(parse(text=generate_model_def(which_is_best,3,dfc3)))))
summary(mylm)





plot(data$STUD.TOTALISCED5_7, data$PERS.TOTACAHC)


# pripraviť generovanie modelov so súhrnom štatistík
# 

data$EXP.CURRTOTAL.EURO = as.numeric(data$EXP.CURRTOTAL.EURO)
data$GDP_pc = as.numeric(data$GDP_pc)
summary(with(data, lm(RANK.OVERALL.TRUE~EXP.CURRTOTAL.EURO+GDP_pc)))




# 0. krivka rank overall vysvetlené pomocou curr personal expenditure (tvar krivky?) 
  # rozdelenie pre LR, nájsť nejaký interval, 
    # pomocou ktorého by sme vizualizovali percentá okolo krivky
  # 100 je max pohľadať alternatívy ku KLR

# 1. model hovorí (aj keď slabý) že medzi jednotlivými školami vrámci země 
  # sú odlišnosti najma vo total expenses a GDP  (rebríček - 1 najlepšie)

# 2. skúmanie rozdielov medzi zeměmi (multilevel/strom na základe)
 # zhlukovka? 
  # neefektivita školství? nezachycuje? zachycuje čo by nemal? 
  # napr VB má rovnomerne nad a pod krivkou, u nás pod krivkou, belgické nad krivkou... čo to znamená?
  # dekomponovanie na jednotlivé ukazatele? Dáva zmysel táto kompozícia? Je niekde neefektivita u nás?
  # Na to potrebujeme krivku

# 3. dá/nedá sa ísť pod svoje umietnenie, a čo zmeniť? skúmať podle zemí na iných faktoroch

# 4. optimalizácia pre štát kam, a ako má štát investovať/pomáhať/promo
 # rôzny target variable pre skúmanie (suma/priemerné umiestnenie, umiestnenie najlepšej školy, ...)

# 5. Doplnenie premenných 
  # a) preskúmať rovnosť príjmov
  # b) ... TODO zamyslieť sa

# 6. časové rady preskúmať ??? TODO
  # udelat nespojité body, v ktorých budú prierezové analýzy
  # posunuli sme sa v niečom oproti stavu napr. pred 5-10 rokmi?

# 7. alternatíva k bodu vyššie
  # chceme koukať na rozdiely ako sa menilo financovanie vysokých škôl vrámci 5 rokov
  # sme schopní posúdiť, ktoré školy zbohatli viac?

# 8. zlepšenie umiestnenia škôl? jednej školy?
  # keď pororvnáme kokrétnu školu s podobne bohatými ale lepšími výsledky, 
    # môžeme určiť v ktorých premenných sa líšia


