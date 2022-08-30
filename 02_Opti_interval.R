
# spustenie 01_Load_data 
source("01_Load_data.R")

x = data$EXP.CURRNONPERSON.EURO/1e7
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

calc_b = opti_b[2]*(1+median(x,na.rm=T))+median(y,na.rm=T)   # b+ med(x)+f(med(x),b) ... atan(b)

plot(x,y)

abline(a=calc_b,b=opti_b[1])

summary(lm(y~x))



plot(x = data$EXP.CURRNONPERSON.EURO/1e7,
     y = data$RANK.OVERALL.TRUE)


ch







span <- function(p) {
  a <- chull(p)                     # Indexes of extremal points, negatively oriented
  e <- p[c(a[-1], a[1]), ,drop=FALSE] - p[a, ] # Edge vectors
  e <- e / sqrt(rowSums(e^2))       # Unit edge dirction vectors
  n <- cbind(e[, 2], -e[, 1])       # Unit normal vectors
  w <- apply(tcrossprod(n, p[a,]), 1, function(x) max(x) - min(x))  # Widths
  i <- which.min(w)                 # Index (into `a`) of best edge
  return(list(origin=p[a[i],], direction=e[i,], normal=n[i,], width=w[i]))
}


n <- 4
set.seed(17)
par(mfrow=c(1,n))
invisible(replicate(n, {
  # Create sample data.
  p <- matrix(na.omit(cbind(data$EXP.CURRNONPERSON.EURO/1e7,
                            data$RANK.OVERALL.TRUE)), ncol=2)
  
  # Find the two lines.
  s <- span(p)
  
  # Convert the result into parameters for `abline`.
  slope <- s$direction[2] / s$direction[1]
  origin.1 <- s$origin
  origin.2 <- s$origin + s$width * s$normal
  
  # Display everything.
  plot(p, pch = 3)
  if (s$direction[1] != 0) {
    abline(a=origin.1[2]-slope*origin.1[1], b=slope, col="Red")
    abline(a=origin.2[2]-slope*origin.2[1], b=slope, col="Gray")
  } else {
    abline(v = c(origin.1[1], origin.2[1]), col=c("Red", "Gray"))
  }
}))
par(mfrow=c(1,1))
