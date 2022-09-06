
# spustenie 01_Load_data 
source("01_Load_data.R")

library(tidyverse)
library(dplyr)


span <- function(d) {
  p <- d[,1:2]
  a <- chull(p)                     # Indexes of extremal points, negatively oriented
  e <- p[c(a[-1], a[1]), ,drop=FALSE] - p[a, ] # Edge vectors
  e <- e / sqrt(rowSums(e^2))       # Unit edge dirction vectors
  n <- cbind(e[, 2], -e[, 1])       # Unit normal vectors
  w <- apply(tcrossprod(n, p[a,]), 1, function(x) max(x) - min(x))  # Widths
  i <- which.min(w)                 # Index (into `a`) of best edge
  return(list(origin=p[a[i],], direction=e[i,], normal=n[i,], width=w[i]))
}

# Display everything.
plot_lines = function(s){
  # Convert the result into parameters for `abline`.
  slope <- s$direction[2] / s$direction[1]
  origin.2 <- s$origin + s$width * s$normal
  
  if (s$direction[1] != 0) {
    abline(a= s$origin[2]-slope * s$origin[1], b=slope, col="Red")
    abline(a= origin.2[2]-slope * origin.2[1], b=slope, col="Gray")
  } else {
    abline(v = c( s$origin[1], origin.2[1]), col=c("Red", "Gray"))
  }
}

get_abline_params <- function(s){
  slope <- s$direction[2] / s$direction[1]
  origin.2 <- s$origin + s$width * s$normal
  
  a1 = s$origin[2]-slope * s$origin[1]
  a2 = origin.2[2]-slope * origin.2[1]
  b = slope
  return(list(a1=a1,a2=a2,b=b))
}

remove_edge = function(s,d){
  slope <- s$direction[2] / s$direction[1]
  origin.2 <- s$origin + s$width * s$normal
  edges = which( round(d[,2],10) == round(s$origin[2]-slope*s$origin[1]+slope*d[,1],10)
               | round(d[,2],10) == round(origin.2[2]-slope*origin.2[1]+slope*d[,1],10))
  wdth = c()
  for (edge in edges){
    wdth = c(wdth,span(d[-edge,])$width)
  }
  return(edges[which.min(wdth)])
  
}


get_s <-  function(d,p){
  n <- nrow(d)
  s <- span(d)
  rme <- c()
  
  while (n*p < nrow(d)){
    rme <- remove_edge(s,d)
    print(d[rme,])
    d <- d[-rme,]
    s <- span(d)
  }
  data <-  data.frame(d)
  colnames(data) <-  c("EXP_EUR","RANK","RN")
  return(list(s=s,data=data))
}


d <- cbind(data$EXP.CURRNONPERSON.EURO/1e7,
           data$RANK.OVERALL.TRUE) %>% 
      na.omit() %>% 
      matrix(ncol=2)
d <-  cbind(d,1:nrow(d))

plot(d, pch = 3)

plot_lines(span(d))

s1.  <- get_s(d,1)
s.9  <- get_s(d, .9)
s.75 <- get_s(d, .75)
s.5  <- get_s(d, .5)

plot_lines(s.9$s)
plot_lines(s.75$s)
plot_lines(s.5$s)

data <-  s1.$data %>% 
  left_join(s.9$data, by="RN") %>% 
  left_join(s.75$data,by="RN") %>% 
  left_join(s.5$data, by="RN") %>% 
  mutate(EXP_EUR = EXP_EUR.x,
         RANK    = RANK.x,
         in.9    = ifelse(is.na(EXP_EUR.y),1,0),
         in.75   = ifelse(is.na(EXP_EUR.x.x),1,0),
         in.5    = ifelse(is.na(EXP_EUR.y.y),1,0)) %>% 
  mutate( GROUP = case_when((in.9+in.75+in.5) == 0 ~ 'FULL',
                            (in.9+in.75+in.5) == 1 ~ '90%',
                            (in.9+in.75+in.5) == 2 ~ '75%',
                            TRUE ~ '50%')) %>% 
  select(EXP_EUR,RANK,GROUP)

abl1.   = get_abline_params(s1.$s)
abl.9  = get_abline_params(s.9$s)
abl.75 = get_abline_params(s.75$s)
abl.5  = get_abline_params(s.5$s)

ggplot(data,aes(x=EXP_EUR, y=RANK, col=GROUP))+
  geom_point()+
  scale_color_manual(values = c("red", "purple", "blue", "green"))+
  geom_abline(slope=abl1.$b,intercept=abl1.$a1,col="red")+
  geom_abline(slope=abl1.$b,intercept=abl1.$a2,col="red")+
  geom_abline(slope=abl.9$b,intercept=abl.9$a1,col="purple")+
  geom_abline(slope=abl.9$b,intercept=abl.9$a2,col="purple")+
  geom_abline(slope=abl.75$b,intercept=abl.75$a1,col="blue")+
  geom_abline(slope=abl.75$b,intercept=abl.75$a2,col="blue")+
  geom_abline(slope=abl.5$b,intercept=abl.5$a1,col="green")+
  geom_abline(slope=abl.5$b,intercept=abl.5$a2,col="green")
  
  


