
# spustenie 01_Load_data 
source("01_Load_data.R")

x = data$EXP.CURRNONPERSON.EURO
y = data$RANK.OVERALL.TRUE
na.action(na.omit(c(1, NA)))

find_opti = function(x,y,p=.5){
  upr_x = x-median(x,na.rm=T)
  upr_y = y-median(y,na.rm=T)
  coeff = summary(lm(upr_y~upr_x))$coefficients
  gen_b0 = rnorm(1000,coeff[1],coeff[3]*3)
  gen_b1 = rnorm(1000,coeff[2],coeff[4]*3)
  
  mat_b0 = matrix(rep(gen_b0,each=length(upr_y)),ncol= length(gen_b0))
  mat_b1 = matrix(upr_x,nrow= length(upr_x)) %*% matrix(gen_b1,ncol= length(gen_x))
  res_mat =mat_b0 + mat_b1
  qs = apply(res_mat,MARGIN=2,function(x) quantile(abs(x-upr_y),p,na.rm=T))
  
  opti_b0 = gen_b0[which.min(qs)]
  opti_b1 = gen_b1[which.min(qs)]
  
  return(c(opti_b0, opti_b1))
}

opti_b = find_opti(x,y,p=.5)

calc_b = opti_b[2]+median(y,na.rm=T) - 65.19969 

plot(x,y)

abline(a=calc_b,b=opti_b[1])






