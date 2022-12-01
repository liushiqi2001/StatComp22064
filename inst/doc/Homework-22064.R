## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
summary(mtcars)

## -----------------------------------------------------------------------------
knitr::kable(mtcars[,1:7])

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
boxplot(mpg ~ cyl,data = mtcars,
        main="Car Mileage Data",
        xlab = "Number of cylinders",
        ylab = "Miles/(US) gallon")

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
n <- 100
set.seed(4)
u <- runif(n)
x <- 2*(1-u)^{-1/2} # F(x) = 1-(2/x)^2, x>=2


for(i in 1:n){
  while(x[i]<2)
  {
    kkk <- runif(1)
    x[i] <- 2*(1-kkk)^{-1/2} 
  }
} # 确保x中的值均大于2



hist(x, prob = TRUE, main = expression(f(x)==8/x^3))
y <- seq(2, 1000, .01)
lines(y, 8/y^3)

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------

num <- 10000
alpha1 <- 8
belta1 <- 2

alpha2 <- 5
belta2 <- 5

alpha3 <- 2
belta3 <- 8

x <- seq(0,1,0.01)

Probability <- dbeta(x,shape1=alpha1,shape2=belta1)

plot(x,Probability,type='l',col='red')
lines(x,dbeta(x,shape1=alpha2,shape2=belta2),col='forestgreen')
lines(x,dbeta(x,shape1=alpha3,shape2=belta3),col='blue3')



## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------

num <- 10000
alpha1 <- 2
belta1 <- 1

alpha2 <- 1
belta2 <- 1

alpha3 <- 1
belta3 <- 2

x <- seq(0,1,0.01)

Probability <- dbeta(x,shape1=alpha1,shape2=belta1)

plot(x,Probability,type='l',col='red')
lines(x,dbeta(x,shape1=alpha2,shape2=belta2),col='forestgreen')
lines(x,dbeta(x,shape1=alpha3,shape2=belta3),col='blue3')



## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------

num <- 10000
alpha1 <- 0.8
belta1 <- 0.2

alpha2 <- 0.5
belta2 <- 0.5

alpha3 <- 0.2
belta3 <- 0.8

x <- seq(0,1,0.01)

Probability <- dbeta(x,shape1=alpha1,shape2=belta1)

plot(x,Probability,type='l',col='red')
lines(x,dbeta(x,shape1=alpha2,shape2=belta2),col='forestgreen')
lines(x,dbeta(x,shape1=alpha3,shape2=belta3),col='blue3')



## -----------------------------------------------------------------------------
n <- 1000
j <- 0
k <- 0
set.seed(1234)
y <- numeric(n)
while (k < n) {
  u <- runif(1)
  j <- j + 1
  x <- runif(1) #random variate from g(.)
  if (x^2 * (1-x) > u) {
    #we accept x
    k <- k + 1
    y[k] <- x 
  } 
}
j # the number of experiments for generating n random numbers

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
hist(y, prob = TRUE, main = expression(f(x)==12*x^2*(1-x)))
yy <- seq(0, 10, .01)
lines(yy, 12*yy^2*(1-yy))

## -----------------------------------------------------------------------------
#compare empirical and theoretical percentiles
p <- seq(.1, .9, .1)
Qhat <- quantile(y, p) #quantiles of sample
Q <- qbeta(p, 3, 2) #theoretical quantiles
se <- sqrt(p^2 * (1-p) / (n * dbeta(Q, 2, 2))) #see Ch. 1


round(rbind(Qhat, Q, se), 3)

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------

n <- 1000
r <- 4 
beta <- 2
lambda <- rgamma(n, r, beta) # lambda is random

#now supply the sample of lambda’s as the Poisson mean
x <- rexp(n, lambda) # the mixture

Xx <- x[1:25]

head(matrix(Xx,5,5))


## -----------------------------------------------------------------------------
n <- 1000
r <- 4 
beta <- 2
lambda <- rgamma(n, r, beta) # lambda is random

#now supply the sample of lambda’s as the Poisson mean
x <- rexp(n, lambda) # the mixture


u <- runif(n)
# the Pareto
y <- 2/(1-u)^{1/4}-2 # F(x) = 1-(2/(2+y))^4, x>=-2


## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
hist(x, prob = TRUE, main = expression(f(x)==64/(2+y)^5))
k <- seq(0, 1000, .01)
lines(k, 64/(2+k)^5)

## -----------------------------------------------------------------------------
quick_sort<-function(x){
  num<-length(x)
  if(num==0||num==1){return(x)
  }else{
    a<-x[1]
    y<-x[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quick_sort(lower),a,quick_sort(upper)))}#??????
}


N <- c(1e4,2e4,4e4,6e4,8e4) 

x <- 100

TTimes <- matrix (0,x,5)

for (k in 1:5){
  n <- N[k]
  Testsample <- matrix(0,n,x)
  Times <- matrix(0,x)
  for (i in 1:x){
    Testsample[1:n,i] <- sample(n)
    Times[i] <- system.time(quick_sort(Testsample[1:n,i]))[1]
  }
  TTimes[1:x,k] <- Times
}



## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
T <- matrix(0,5)

for  (i in 1:5){
  T[i] <- N[i]*log10(N[i])
}


A <- colMeans(TTimes)

Lm <- lm(A~T)

lie1 <- matrix(A)
lie2 <- matrix(T)
data <- matrix(0,5,2)
data[1:5,1] <- lie1
data[1:5,2] <- lie2
data <- data.frame(data)

LM <- lm(A~T)

library(ggplot2)
ggplot(data, aes(X2, X1))+
  geom_point(size=6,shape=21)+
  geom_smooth(method = "lm")+
  labs(x="nlog(n)",y="an")




## -----------------------------------------------------------------------------

Co <- exp(1)-(exp(1)-1)^2


## -----------------------------------------------------------------------------

VareU <- (1/2)*(exp(2)-1)-(exp(1)-1)^2
Vare1_U <- (1/2)*(exp(2)-1)-(1-exp(1))^2


## -----------------------------------------------------------------------------

Vartherahat1 <- (1/4)*(exp(2)-1)-(1/2)*(exp(1)-1)^2


## -----------------------------------------------------------------------------

Vartherahat2 <- (1/4)*(exp(2)-1)+(1/2)*exp(1)-(exp(1)-1)^2


## -----------------------------------------------------------------------------

rdu <- (Vartherahat1-Vartherahat2)/Vartherahat1


## -----------------------------------------------------------------------------

MC.Phi <- function(x, R = 10000, antithetic = FALSE) {
  u <- runif(R/2)
  if (antithetic) v <- 1 - u else v <- runif(R/2)
  u <- c(u, v)
  g <- x*exp(u) # x*exp(u) ~ U(0,x)
  cdf <- mean(g)
  cdf
}


m <- 1000

MC1 <- MC2 <- numeric(m)

x <- 1
for (i in 1:m) {
  MC1[i] <- MC.Phi(x, R = 1000, antithetic = FALSE)
  MC2[i] <- MC.Phi(x, R = 1000, antithetic = TRUE)
}

mean1 <- mean(MC1)
mean2 <- mean(MC2)

var1 <- var(MC1)
var2 <- var(MC2)

rdu2 <- c((var(MC1)-var(MC2))/var(MC1))

di <- abs(rdu-rdu2)


## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
x <- seq(1, 10, .01)
g <- x^2*exp(-x^2/2)/(2*pi)^0.5
plot(x,g,type = "l", ylim = c(0, 0.4))

## -----------------------------------------------------------------------------
x <- seq(1, 10, .01)
g <- x^2*exp(-x^2/2)/(2*pi)^0.5

mu <- (2)^0.5
d1 <- 100000
S1 <- 1000
for (s in seq(0.5, 10, .01)){
  f1 <- dnorm(x, mean = mu, sd = s)
  d <- var(g/f1)
  # try to find f1 that the ratio g(x)/f1(x) is nearly constant.
  if(d < d1){ 
    d1 <- d
    S1 <- s
  }
}
f1 <- dnorm(x, mean = mu, sd = S1)

## -----------------------------------------------------------------------------
x <- seq(1, 10, .01)
g <- x^2*exp(-x^2/2)/(2*pi)^0.5

d2 <- 100000
AL2 <- 1000
for (al in seq(1.01, 10, .01)){
  be <- (al-1)/2^0.5
  f2 <- dgamma(x, shape = al, rate = be)
  d <- var(g/f2)
  # try to find f2 that the ratio g(x)/f2(x) is nearly constant.
  if(d < d2){
    d2 <- d
    AL2 <- al
  }
}
BE2 <- (AL2-1)/2^0.5
f2 <- dgamma(x, shape = AL2, rate = BE2)

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
y <- g
plot(x,y,type = "l", ylim = c(0, 0.7))
lines(x,f1,lty = 6)
lines(x,f2,lty = 2)
legend("topright", inset = 0.02, legend = c("g(x)", "f1","f2"), lty = c(1,6,2))


## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
gg <- 10000*rep(1,length(x))
fg1 <- g/f1
fg2 <- g/f2

plot(x,gg,ylim = c(0, 0.8))
lines(x,fg1,lty = 1)
lines(x,fg2,lty = 2)
legend("topright", inset = 0.02, legend = c("fg1","fg2"), lty = c(1,2))

## -----------------------------------------------------------------------------

m <- 10000
g <- function(x) {
  exp(-x-log(1+x^2))*(x>0)*(x<1)
}
set.seed(12)
u <- runif(m) 
# f3 with the inverse transform method
x <- - log(1-u*(1-exp(-1))) # F=(1-exp(-x))/(1-exp(-1)), x=-log(1-F*(1-exp(-1)))
fg1 <- g(x)/(exp(-x)/(1-exp(-1)))
theta.hat1 <- mean(fg1) # theta.hat without stratification
se1 <- sd(fg1) # variance without stratification

M <- 10000
k <- 5 
r <- M/k # replicates per stratum
set.seed(12)
T1 <- numeric(k)
vvar1 <- matrix(0, 1, k) 
f1 <- function(x)k*exp(-x)/(1-exp(-1))*(x>0)*(x<1)

for (j in 1:k) {
  xx <- - log(1 - runif(M/k,(j-1)/k,j/k) * (1 - exp(-1)))
  FG1 <- g(xx)/f1(xx)
  T1[j]<-mean(FG1)
  vvar1[j]<-var(FG1)
}
theta.hat2 <- sum(T1) # theta.hat with stratification
se2 <- sqrt(mean(vvar1)) # variance with stratification


## -----------------------------------------------------------------------------
re1 <- rbind(c(theta.hat1,theta.hat2), c(se1,se2))
result1 <- data.frame(re1)
names(result1)<-c("Without stratification","With stratification")
row.names(result1) <- c('theta.hat','se')
knitr::kable (result1, align="c")

## -----------------------------------------------------------------------------
N <- 50 #number of times to repeat the estimation

M <- 10000
set.seed(12)
estg2 <- matrix(0, N, 1) 
#stdg2 <- matrix(0, N, 1) 
g <- function(x) {
  exp(-x-log(1+x^2))*(x>0)*(x<1)
}
for (i in 1:N) {
  u <- runif(M) 
  # f3 with the inverse transform method
  x <- - log(1-u*(1-exp(-1))) # F=(1-exp(-x))/(1-exp(-1)), x=-log(1-F*(1-exp(-1)))
  fg1 <- g(x)/(exp(-x)/(1-exp(-1)))
  estg2[i, 1] <- mean(fg1) 
  #stdg2[i, 1] <- sd(fg1)
}

theta.hat1 <- mean(estg2) # theta.hat without stratification
se1 <- sd(estg2) # variance without stratification

M <- 10000
k <- 5
r <- M/k #replicates per stratum
set.seed(12)
T1 <- numeric(k)
est1 <- matrix(0, N, 1) 
#stdd1 <- matrix(0, N, 1) 
f1 <- function(x)k*exp(-x)/(1-exp(-1))*(x>0)*(x<1)

for (i in 1:N) {
  for (j in 1:k) {
    xx <- - log(1 - runif(M/k,(j-1)/k,j/k) * (1 - exp(-1)))
    FG1 <- g(xx)/f1(xx)
    T1[j]<-mean(FG1)
  }
  est1[i, 1] <- sum(T1) 
  #stdd1[i, 1] <- sd(T1) 
}
theta.hat2 <- mean(est1) # theta.hat with stratification before modification
se2 <- sd(est1) # variance without stratification before modification

set.seed(12)
T2 <- numeric(k)
est2 <- matrix(0, N, 1) 
#stdd2 <- matrix(0, N, 1)
f2 <- function(x)exp(-x)/(1-exp(-1))*(x>0)*(x<1)
for (i in 1:N) {
  for (j in 1:k) {
    xx <- - log(1 - runif(M/k,(j-1)/k,j/k) * (1 - exp(-1)))
    FG2 <- g(xx)/f2(xx)
    T2[j]<-mean(FG2)
  }
  est2[i, 1] <- mean(T2) 
  #stdd2[i, 1] <- sd(T2)
}
theta.hat3 <- mean(est2) # theta.hat with stratification after modification
se3 <- sd(est2) # variance with stratification after modification


## -----------------------------------------------------------------------------
re2 <- rbind(c(theta.hat1,theta.hat2,theta.hat3), c(se1,se2,se3))
result2 <- data.frame(re2)
names(result2)<-c("Without stratification","With stratification before modification","With stratification after modification")
row.names(result2) <- c('theta.hat','se')
knitr::kable (result2, align="c")

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
rm(list = ls())
SAmple1 <- function(m,n){
  # n: the number of samples in one simulation
  # m: the number of simulations
  set.seed(126)
  y <- matrix(0,m,n) # Each row is a simulated sample of size n
  for (i in 1:m){
    x <- rlnorm(n)
    y[i,1:n] <- log(x)
  }
  y
}


Confidenceint <- function(Samp){
  # Samp: an m * n matrix, including all the samples of the simulation
  m <- nrow(Samp)
  n <- ncol(Samp)
  CI <- matrix(0,m,2)
  for (i in 1:m){
    y <- Samp[i,1:n]
    yhat <- mean(y)
    se <- sd(y)/sqrt(n)
    low <- yhat + se * qnorm(c(0.025))
    up <- yhat + se * qnorm(c(0.975)) 
    CI[i,1] <- low
    CI[i,2] <- up
  }
  CI
}



REsult1 <- function(confidenceint){
  # confidenceint: an m * 2 matrix, including all the confidence intervals in the simulation
  m <- nrow(confidenceint)
  LOW <- confidenceint[1:m,1]
  UP <- confidenceint[1:m,2]
  res <- mean(LOW < 0 & UP > 0)
  res
}

set.seed(126)

m <- 10000
n <- 100
sam <- SAmple1(m,n)
ci <- Confidenceint(sam)
re <- REsult1(ci) # an empirical estimate of the confidence level

# output the result
Cii <- data.frame(ci)
names(Cii) <- c('Low bound','Upper bound')
knitr::kable (head(Cii,6),align="c")
re


## -----------------------------------------------------------------------------

rm(list = ls())

SAmple2 <- function(sigma1,sigma2,m,n){
  set.seed(12)
  Samp1 <- matrix(0,m,n)
  Samp2 <- matrix(0,m,n)
  for (i in 1:m){
    x <- rnorm(n, 0, sigma1)
    y <- rnorm(n, 0, sigma2)
    Samp1[i,1:n] <- x
    Samp2[i,1:n] <- y
  }
  SAmp <- rbind(Samp1,Samp2)
  SAmp
}


COunt5test <- function(x, y) {
  X <- x - mean(x)
  Y <- y - mean(y)
  outx <- sum(X > max(Y)) + sum(X < min(Y))
  outy <- sum(Y > max(X)) + sum(Y < min(X))
  cou5te <- as.integer(max(c(outx, outy)) > 5)
  cou5te
}

ANalysis2 <- function(Samp) {
  m <- nrow(Samp)/2
  n <- ncol(Samp)
  Test <- matrix(0,2,m)
  for (i in 1:m){
    x <- Samp[i,1:n]
    y <- Samp[m+i,1:n]
    C5 <- COunt5test(x, y)
    Fp <- var.test(x, y)$p.value
    Ftest <- as.integer(Fp <= 0.055)
    Test[1:2,i] <- c(C5, Ftest)
  }
  Test
}

REsult2 <- function(Test) {
  RE <- c(n,rowMeans(Test))
  RE
}

sigma1 <- 1
sigma2 <- 1.5
m <- 10000
N <- c(20, 30, 60, 90, 200, 500)
K <- length(N)
REsu <- matrix(0,K,3)
for (k in 1:K){
  n <- N[k]
  samp <- SAmple2(sigma1,sigma2,m,n)
  test <- ANalysis2(samp)
  REsu[k,1:3] <- REsult2(test)
}

resu <- data.frame(REsu)
names(resu) <- c('n','Power of COunt5test','Power of Ftest')
knitr::kable (resu,align="c")


## -----------------------------------------------------------------------------
rm(list = ls())

library(boot)

Sample1 <- function(x){
  samp <- aircondit[x]
  samp
}

Rate1 <- function(samp, i) {
  rat <- 1/mean(as.matrix(samp[i, ]))
  rat
}

Result1 <- function(samp,func,Rr){
  bo <- boot(samp, statistic = func, R = Rr)
  print(bo)
}

set.seed(1234)
samp <- Sample1(1)
resu <- Result1(samp,Rate1,2000)

detach(package:boot)

rm(list = ls())

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
rm(list = ls())

library(boot)

Sample2 <- function(x){
  samp <- aircondit[x]
  samp
}

Meant2 <- function(x, i) {
  mea <- mean(as.matrix(x[i, ]))
  mea
}

Result2 <- function(samp,func,Rr){
  bo <- boot(samp, statistic = func, R = Rr)
  re <- boot.ci(bo, type = c("norm", "perc", "basic", "bca"))
  print(bo)
  print(re)
  hist(bo$t, prob = TRUE, main = " ")
  points(bo$t0, 0, cex = 2, pch = 16)
  bo
}

set.seed(1234)
samp <- Sample2(1)
resu <- Result2(samp,Meant2,2000)

detach(package:boot)

rm(list = ls())


## -----------------------------------------------------------------------------
rm(list = ls())

skewness <- function(x,i) {
  #computes the sample skewness coeff.
  x_bar <- mean(x[i])
  x_bar
}

Sample3 <- function(n, mea, sd){
  samp <- rnorm(n, mea, sd)
  samp
}

Analysis3 <- function(m, func, Rr, n, mea, sd){
  library(boot)
  nornorm <- matrix(0, m, 2)
  norbasi <- matrix(0, m, 2)
  norperc <- matrix(0, m, 2)
  for (i in 1:m) {
    Samp <- Sample3(n, mea, sd)
    Skew <- boot(Samp, statistic = func, R=Rr)
    Nor <- boot.ci(Skew, type=c("norm","basic","perc"))
    nornorm[i,] <- Nor$norm[2:3]
    norbasi[i,] <- Nor$basic[4:5]
    norperc[i,] <- Nor$percent[4:5]
  }
  #Calculate the coverage probability of a normal distribution
  norm <- mean(nornorm[,1] <= s & nornorm[,2] >= s)
  basi <- mean(norbasi[,1] <= s & norbasi[,2] >= s)
  perc <- mean(norperc[,1] <= s & norperc[,2] >= s)
  #Calculate the probability of the left side of the normal distribution
  normleft <- mean(nornorm[,1] >= s )
  basileft <- mean(norbasi[,1] >= s )
  percleft <- mean(norperc[,1] >= s )
  #Calculate the right side probability of a normal distribution
  normright <- mean(nornorm[,2] <= s )
  basiright <- mean(norbasi[,2] <= s )
  percright <- mean(norperc[,2] <= s )
  analyresu <- c(norm, basi, perc, normleft, basileft, percleft, normright, basiright, percright)
  analyresu
}

Result3 <- function(sd, analyresu){
  dnam <- paste("N ( 0 ,", as.character(sd^2),")",seq="")
  Distribution <- c(dnam)
  Type <- c("basic", "norm", "perc")
  Left <- analyresu[4:6]
  Right <- analyresu[7:9]
  P.coverage <- analyresu[1:3]
  result <- data.frame(Distribution, Type, Left, Right, P.coverage)
  result
}

s <- 0
n <- 20
m <- 1000
R <- 1000

mea <- 0
sd <- 3 

# We can set n, m, R, mea, sd any way we want.

set.seed(1234)
library(boot)

Analyresu <- Analysis3(m, skewness, R, n, mea, sd)
Resu <- Result3(sd, Analyresu)

knitr::kable (Resu, align="c")

rm(list = ls())

## -----------------------------------------------------------------------------
rm(list = ls())

set.seed(1234)

library(bootstrap)
attach(scor)

Sample1 <- function(scor){
  x <- as.matrix(scor)
  return(x)
}

Analysis1 <- function(x){
  n <- nrow(x)
  theta_jack <- numeric(n)
  lambda <- eigen(cov(x))$values
  theta_hat <- max(lambda/sum(lambda))
  for (i in 1:n) {
    y <- x[-i, ]
    s <- cov(y)
    lambda <- eigen(s)$values
    theta_jack[i] <- max(lambda/sum(lambda))
  }
  bias_jack <- (n - 1) * (mean(theta_jack) - theta_hat)
  se_jack <- sqrt((n - 1)/n * sum((theta_jack - mean(theta_jack))^2)) 
  
  re <- c(theta_hat, bias_jack, se_jack)
  
  return(re)
}

Result1 <- function(theta_hat, bias_jack, se_jack){
  Re1 <- data.frame(theta_hat, bias_jack, se_jack)
  names(Re1) <- c('est','bias','se')
  return(Re1)
}

x <- Sample1(scor)
anRE <- Analysis1(x)
Re1 <- Result1(anRE[1],anRE[2],anRE[3])

knitr::kable (Re1,align="c")

set.seed(1234)

attach(scor)

th <- function(x, i) {
  y <- as.matrix(x[i, ])
  s <- cov(y)
  e <- eigen(s)
  lambda <- e$values
  max(lambda/sum(lambda))
}

Result1_2 <- function(scor){
  library(boot)
  boot(scor, statistic = th, R = 2000)
}

Re2 <- Result1_2(scor)
Re2

rm(list = ls())

## -----------------------------------------------------------------------------
rm(list = ls())

set.seed(1234)

library(DAAG, warn.conflict = FALSE)
attach(ironslag)

Analysis2 <- function(magnetic){
  n <- length(magnetic)
  N <- choose(n, 2)
  e1 <- numeric(N)
  e2 <- numeric(N)
  e3 <- numeric(N)
  e4 <- numeric(N)
  e5 <- numeric(N)
  ij <- 1
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    k <- c(i, j)
    y <- magnetic[-k]
    x <- chemical[-k]
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
    e1[ij] <- sum((magnetic[k] - yhat1)^2)
    J2 <- lm(y ~ x + I(x^2))
    
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
      J2$coef[3] * chemical[k]^2
    e2[ij] <- sum((magnetic[k] - yhat2)^2)
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
    yhat3 <- exp(logyhat3)
    e3[ij] <- sum((magnetic[k] - yhat3)^2)
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
    yhat4 <- exp(logyhat4)
    e4[ij] <- sum((magnetic[k] - yhat4)^2)
    c2 <- x^2
    c3 <- x^3
    J5 <- lm(y ~ x + c2 + c3)
    yhat5 <- J5$coef[1] + J5$coef[2] * chemical[k] +
      J5$coef[3] * chemical[k]^2 + J5$coef[4] * chemical[k]^3
    e5[ij] <- sum((magnetic[k] - yhat5)^2)
    ij <- ij + 1
  }
  Re <- c(sum(e1), sum(e2), sum(e3), sum(e4), sum(e5))/N
  return(Re)
}

Re <- Analysis2(magnetic)
Re

rm(list = ls())


## -----------------------------------------------------------------------------
rm(list = ls())

set.seed(433)

spear.perm <- function(x, y) {
  stest <- cor.test(x, y, method = "spearman")
  n <- length(x)
  rs <- replicate(R, expr = {
    k <- sample(1:n)
    cor.test(x, y[k], method = "spearman")$estimate
  })
  rs1 <- c(stest$estimate, rs)
  pval <- mean(as.integer(stest$estimate <= rs1))
  return(list(rho.s = stest$estimate, p.value = pval))
}

set.seed(4321)

library(MASS)
mu <- c(0, 0)
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
n <- 30
R <- 499

Sample3 <-function(n, mu, Sigma){
  x <- mvrnorm(n, mu, Sigma)
  return(x)
}
x <- Sample3(n, mu, Sigma)
cor.test(x[, 1], x[, 2], method = "spearman")
spear.perm(x[, 1], x[, 2])


rm(list = ls())

## -----------------------------------------------------------------------------
rm(list = ls())

set.seed(123451)

Laplacerw <- function(N, x0, sigma) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k <- 0
  for (i in 2:N) {
    xt <- x[i - 1]
    y <- rnorm(1, xt, sigma)
    if (u[i] <= exp(abs(xt) - abs(y)))
      x[i] <- y
    else {
      x[i] <- x[i - 1]
      k <- k + 1
    }
  }
  return(list(x = x, k = k))
}

N <- 5500
sigma <- c(0.5, 1, 2, 5)
x0 <- rnorm(1)
rw1 <- Laplacerw(N, x0, sigma[1])
rw2 <- Laplacerw(N, x0, sigma[2])
rw3 <- Laplacerw(N, x0, sigma[3])
rw4 <- Laplacerw(N, x0, sigma[4])

rejectionrates <- c(rw1$k, rw2$k, rw3$k, rw4$k)/N
acceptancerates <- rep(1,4)-rejectionrates

Rates <- data.frame(rbind(rejectionrates, acceptancerates))
names(Rates)<-c('Chain1','Chain2','Chain3','Chain4')
row.names(Rates) <- c('Rejection rates','Acceptance rates')
knitr::kable (Rates, align="c")

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
b <- 100
y1 <- rw1$x[(b + 1):N]
y2 <- rw2$x[(b + 1):N]
y3 <- rw3$x[(b + 1):N]
y4 <- rw4$x[(b + 1):N]


#par(mfrow = c(2, 2))
plot(rw1$x, type = "l")
plot(rw2$x, type = "l")
plot(rw3$x, type = "l")
plot(rw4$x, type = "l")
#par(mfrow = c(1, 1))


#par(mfrow = c(2, 2))
p <- ppoints(200)
y <- qexp(p, 1)
z <- c(-rev(y), y)
k <- 4
fx <- 0.5 * exp(-abs(z))
hist(y1, breaks = "Scott", freq = FALSE, ylim = c(0,0.5))
lines(z, fx)
hist(y2, breaks = "Scott", freq = FALSE, ylim = c(0,0.5))
lines(z, fx)
hist(y3, breaks = "Scott", freq = FALSE, ylim = c(0,0.5))
lines(z, fx)
hist(y4, breaks = "Scott", freq = FALSE, ylim = c(0,0.5))
lines(z, fx)
#par(mfrow = c(1, 1))


#par(mfrow = c(2, 2))
Q1 <- quantile(y1, p)
qqplot(z, Q1, cex = 0.4)
abline(0, 1)
Q2 <- quantile(y2, p)
qqplot(z, Q2, cex = 0.4)
abline(0, 1)
Q3 <- quantile(y3, p)
qqplot(z, Q3, cex = 0.4)
abline(0, 1)
Q4 <- quantile(y4, p)
qqplot(z, Q4, cex = 0.4)
abline(0, 1)
#par(mfrow = c(1, 1))

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------

allchain <- t(as.matrix(data.frame(y1,y2,y3,y4)))

Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  
  psi.means <- rowMeans(psi)     #row means
  B <- n * var(psi.means)        #between variance est.
  psi.w <- apply(psi, 1, "var")  #within variances
  W <- mean(psi.w)               #within est.
  v.hat <- W*(n-1)/n + B/n+(B/(n*k))     #upper variance est.
  r.hat <- v.hat / W             #G-R statistic
  return(r.hat)
}

#compute diagnostic statistics
psi <- t(apply(allchain, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))
r_hat <- Gelman.Rubin(psi)


#plot psi for the four chains
#par(mfrow=c(2,2))
for (i in 1:k)
  plot(psi[i,1:ncol(psi)], type="l",
       xlab=i, ylab=bquote(psi))
#par(mfrow=c(1,1)) #restore default

#plot the sequence of R-hat statistics
rhat <- rep(0, ncol(psi))
for (j in 1:ncol(psi))
  rhat[j] <- Gelman.Rubin(psi[,1:j])
plot(rhat[1:ncol(psi)], type="l", xlab="", ylab="R",ylim=c(1,3.5))
abline(h=1.2, lty=2)

rm(list = ls())

## -----------------------------------------------------------------------------
#rm(list = ls())

set.seed(12340)

N <- 5000
burn <- 1000
rho <- 0.9

k <- 4
MU1 <- c(0,1,-1,2)
MU2 <- c(0,1,-1,2)

Mu1chain <- matrix(0,k,(N-burn))
Mu2chain <- matrix(0,k,(N-burn))

for (kkk in (1:4)){
  X <- matrix(0, N, 2)
  mu1 <- 0
  mu2 <- 0
  sigma1 <- 1
  sigma2 <- 1
  s1 <- sqrt(1 - rho^2) * sigma1
  s2 <- sqrt(1 - rho^2) * sigma2
  X[1, ] <- c(MU1[kkk], MU2[kkk])
  for (i in 2:N) {
    x2 <- X[i - 1, 2]
    m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
    X[i, 1] <- rnorm(1, m1, s1)
    x1 <- X[i, 1]
    m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
    X[i, 2] <- rnorm(1, m2, s2)
  }
  b <- burn + 1
  x <- X[b:N, ]
  Mu1chain[kkk, ] <- x[, 1]
  Mu2chain[kkk, ] <- x[, 2]
}

k <- 4
Linearmodel <- matrix(0,k,2)

for (kk in (1:k)){
  L <- lm(Mu2chain[kk,] ~ Mu1chain[kk,])
  Linearmodel[kk,] <- L$coefficients
}

Linearpa <- data.frame(Linearmodel)
names(Linearpa)<-c("beta0","beta1")
row.names(Linearpa) <- c('chain1','chain2','chain3','chain4')
knitr::kable (Linearpa, align="c")

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------

#par(mfrow=c(2,2))
for (kk in (1:k)){
  plot(Mu1chain[kk,], Mu2chain[kk,], cex = 0.25,xlab = 'x',ylab = 'y')
  abline(h = 0, v = 0)
}
#par(mfrow=c(1,1)) #restore default


#par(mfrow=c(2,2))
for (kk in (1:k)){
  L <- lm(Mu2chain[kk,] ~ Mu1chain[kk,])
  plot(L$fit, L$res, cex = 0.25)
  abline(h = 0)
}
#par(mfrow=c(1,1)) #restore default


#par(mfrow=c(2,2))
for (kk in (1:k)){
  L <- lm(Mu2chain[kk,] ~ Mu1chain[kk,])
  qqnorm(L$res, cex = 0.25)
  qqline(L$res)
}
#par(mfrow=c(1,1)) #restore default

Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  
  psi.means <- rowMeans(psi)     #row means
  B <- n * var(psi.means)        #between variance est.
  psi.w <- apply(psi, 1, "var")  #within variances
  W <- mean(psi.w)               #within est.
  v.hat <- W*(n-1)/n + B/n+(B/(n*k))     #upper variance est.
  r.hat <- v.hat / W             #G-R statistic
  return(r.hat)
}

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
allchain1 <- as.matrix(data.frame(Mu1chain))


#compute diagnostic statistics
psi1 <- t(apply(allchain1, 1, cumsum))
for (i in 1:nrow(psi1))
  psi1[i,] <- psi1[i,] / (1:ncol(psi1))
r_hat1 <- Gelman.Rubin(psi1)

allchain2 <- as.matrix(data.frame(Mu2chain))


#compute diagnostic statistics
psi2 <- t(apply(allchain2, 1, cumsum))
for (i in 1:nrow(psi2))
  psi2[i,] <- psi2[i,] / (1:ncol(psi2))
r_hat2 <- Gelman.Rubin(psi2)

k <- 4


#plot psi for the four chains
#par(mfrow=c(2,2))
for (i in 1:k)
  plot(psi1[i,1:ncol(psi1)], type="l",xlab=i, ylab=bquote(psi1))
#par(mfrow=c(1,1)) #restore default


#par(mfrow=c(2,2))
for (i in 1:k)
  plot(psi2[i,1:ncol(psi2)], type="l",xlab=i, ylab=bquote(psi2))
#par(mfrow=c(1,1)) #restore default


#plot the sequence of R-hat statistics
#par(mfrow=c(1,2))
rhat1 <- rep(0, ncol(psi1))
for (j in 1:ncol(psi1))
  rhat1[j] <- Gelman.Rubin(psi1[,1:j])
plot(rhat1[1:ncol(psi1)], type="l", xlab="", ylab="R",ylim=c(1,3.5))
abline(h=1.2, lty=2)

rhat2 <- rep(0, ncol(psi2))
for (j in 1:ncol(psi2))
  rhat2[j] <- Gelman.Rubin(psi2[,1:j])
plot(rhat2[1:ncol(psi2)], type="l", xlab="", ylab="R",ylim=c(1,3.5))
abline(h=1.2, lty=2)
#par(mfrow=c(1,1))

rm(list = ls())

## ----include=FALSE------------------------------------------------------------
rm(list = ls())
library(bda)

## -----------------------------------------------------------------------------
# Set parameters
N <- 5000 # size of sample
Pr <- 2000 # number of replicates
aM <- 1
aY <- 1

################ Case1: alpha = 0; beta = 1 ################

##### 1.1 Data generation
alpha <- 0
beta <- 1
gamma <- 1
set.seed(12345)
eM <- rnorm(N,0,1)
eY <- rnorm(N,0,1)
X <- rnorm(N,0,1)
M <- aM*rep(1,N) + alpha*X + eM
Y <- aY*rep(1,N) + beta*M + gamma*X + eY

##### 1.2 Data analysis
# Obtain statistics and p-value
T0 <- mediation.test(M,X,Y)$Sobel[1]
P0_1 <- mediation.test(M,X,Y)$Sobel[2]
# Permutation test
T <- matrix(0,1,Pr)
for (k in 1:Pr){
  i_i <-  sample(1:N, N, replace = FALSE)
  PX <- matrix(0,1,N)
  for (i in 1:N){
    PX[i] <- X[i_i[i]]
  }
  PX <- as.numeric(PX)
  t <- mediation.test(M,PX,Y)$Sobel[1]
  T[k] <- t
}
# p-value by permutation test
P1_1 <- sum(abs(T)>abs(T0))/Pr

################ Case2: alpha = 1; beta = 0 ################

##### 2.1 Data generation
alpha <- 1
beta <- 0
gamma <- 1
set.seed(1242)
eM <- rnorm(N,0,1)
eY <- rnorm(N,0,1)
X <- rnorm(N,0,1)
M <- aM*rep(1,N) + alpha*X + eM
Y <- aY*rep(1,N) + beta*M + gamma*X + eY

##### 2.2 Data analysis
# Obtain statistics and p-value
T0 <- mediation.test(M,X,Y)$Sobel[1]
P0_2 <- mediation.test(M,X,Y)$Sobel[2]
# Permutation test
T <- matrix(0,1,Pr)
for (k in 1:Pr){
  i_i <-  sample(1:N, N, replace = FALSE)
  PY <- matrix(0,1,N)
  for (i in 1:N){
    PY[i] <- Y[i_i[i]]
  }
  PY <- as.numeric(PY)
  t <- mediation.test(M,X,PY)$Sobel[1]
  T[k] <- t
}
# p-value by permutation test
P1_2 <- sum(abs(T)>abs(T0))/Pr

################ Case3: alpha = 0; beta = 0 ################

##### 3.1 Data generation
alpha <- 0
beta <- 0
gamma <- 1
set.seed(12345)
eM <- rnorm(N,0,1)
eY <- rnorm(N,0,1)
X <- rnorm(N,0,1)
M <- aM*rep(1,N) + alpha*X + eM
Y <- aY*rep(1,N) + beta*M + gamma*X + eY

##### 3.2 Data analysis
# Obtain statistics and p-value
T0 <- mediation.test(M,X,Y)$Sobel[1]
P0_3 <- mediation.test(M,X,Y)$Sobel[2]
# Permutation test
T <- matrix(0,1,Pr)
for (k in 1:Pr){
  i_i <-  sample(1:N, N, replace = FALSE)
  PX <- matrix(0,1,N)
  for (i in 1:N){
    PX[i] <- X[i_i[i]]
  }
  PX <- as.numeric(PX)
  j_j <-  sample(1:N, N, replace = FALSE)
  PY <- matrix(0,1,N)
  for (j in 1:N){
    PY[j] <- Y[j_j[j]]
  }
  PY <- as.numeric(PY)
  t <- mediation.test(M,PX,PY)$Sobel[1]
  T[k] <- t
}
# p-value by permutation test
P1_3 <- sum(abs(T)>abs(T0))/Pr


##### 3 Result reporting
P0 <- c(P0_1,P0_2,P0_3)
P1 <- c(P1_1,P1_2,P1_3)
result <- data.frame(t(cbind(P0,P1)))
names(result)<-c("alpha=0,beta=1","alpha=1,beta=0","alpha=0,beta=0")
row.names(result) <- c('p-value wiht H0','p-value with permutation test')
knitr::kable (result, align="c")

rm(list = ls())

## -----------------------------------------------------------------------------
rm(list = ls())
ALPHA <- function(N,b1,b2,b3,f0) {
  alpha <- matrix(0,1:length(f0))
  for (i in 1:length(f0)){
    f_0 <- f0[i]
    g <- function(alp){
      tmp <- exp(-alp-b1*x1-b2*x2-b3*x3)
      p <- 1/(1+tmp)
      mean(p) - f_0
    }
    solution <- uniroot(g,c(-20,20))
    alpha[i] <- solution$root
  }
  alpha
}

## -----------------------------------------------------------------------------
##### Set parameters
N <- 1e6;
b1 <- 0; 
b2 <- 1; 
b3 <- -1
f0 <- c(0.1,0.01,0.001,0.0001)

##### Data generation
set.seed(12345)
x1 <- rpois(N, 1)
x2 <- rexp(N, 1)
x3 <- rbinom(N,1,0.5)

##### Data analysis
alpha <- ALPHA(N,b1,b2,b3,f0)

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
##### Result reporting
plot(-log(f0),alpha)
rm(list = ls())

## ----fig.align ='center',fig.height=3.5, fig.width=5.5------------------------
U <- c(11,8,27,13,16,0,23,10,24,2)
V <- c(12,9,28,14,17,1,24,11,25,3)

# Function with equation
fun <- function(x) {-(U[1]*exp(-x*U[1])-V[1]*exp(-x*V[1]))/(exp(-x*U[1])-exp(-x*V[1])) - (U[2]*exp(-x*U[2])-V[2]*exp(-x*V[2]))/(exp(-x*U[2])-exp(-x*V[2])) - (U[3]*exp(-x*U[3])-V[3]*exp(-x*V[3]))/(exp(-x*U[3])-exp(-x*V[3])) - (U[4]*exp(-x*U[4])-V[4]*exp(-x*V[4]))/(exp(-x*U[4])-exp(-x*V[4])) - (U[5]*exp(-x*U[5])-V[5]*exp(-x*V[5]))/(exp(-x*U[5])-exp(-x*V[5])) - (U[6]*exp(-x*U[6])-V[6]*exp(-x*V[6]))/(exp(-x*U[6])-exp(-x*V[6])) - (U[7]*exp(-x*U[7])-V[7]*exp(-x*V[7]))/(exp(-x*U[7])-exp(-x*V[7])) - (U[8]*exp(-x*U[8])-V[8]*exp(-x*V[8]))/(exp(-x*U[8])-exp(-x*V[8])) - (U[9]*exp(-x*U[9])-V[9]*exp(-x*V[9]))/(exp(-x*U[9])-exp(-x*V[9])) - (U[10]*exp(-x*U[10])-V[10]*exp(-x*V[10]))/(exp(-x*U[10])-exp(-x*V[10]))}
  

curve(fun, from = 0, to = 1); abline(h = 0, lty = 3)

## -----------------------------------------------------------------------------
mle1 <- uniroot(fun, lower = 0, upper = 0.2)
mle1

## -----------------------------------------------------------------------------
rm(list = ls())
U <- c(11,8,27,13,16,0,23,10,24,2)
V <- c(12,9,28,14,17,1,24,11,25,3)

EM <- function(U,V,max.it=10000,eps=1e-8){
  n <- length(U)
  i <- 1
  theta1 <- 0.01
  theta2 <- 0.02
  while( abs(theta1 - theta2) >= eps){
    theta1 <- theta2
    fun <- function(x) {(U[1]*exp(-x*U[1])-V[1]*exp(-x*V[1]))/(exp(-x*U[1])-exp(-x*V[1])) + (U[2]*exp(-x*U[2])-V[2]*exp(-x*V[2]))/(exp(-x*U[2])-exp(-x*V[2])) + (U[3]*exp(-x*U[3])-V[3]*exp(-x*V[3]))/(exp(-x*U[3])-exp(-x*V[3])) + (U[4]*exp(-x*U[4])-V[4]*exp(-x*V[4]))/(exp(-x*U[4])-exp(-x*V[4])) + (U[5]*exp(-x*U[5])-V[5]*exp(-x*V[5]))/(exp(-x*U[5])-exp(-x*V[5])) + (U[6]*exp(-x*U[6])-V[6]*exp(-x*V[6]))/(exp(-x*U[6])-exp(-x*V[6])) + (U[7]*exp(-x*U[7])-V[7]*exp(-x*V[7]))/(exp(-x*U[7])-exp(-x*V[7])) + (U[8]*exp(-x*U[8])-V[8]*exp(-x*V[8]))/(exp(-x*U[8])-exp(-x*V[8])) + (U[9]*exp(-x*U[9])-V[9]*exp(-x*V[9]))/(exp(-x*U[9])-exp(-x*V[9])) + (U[10]*exp(-x*U[10])-V[10]*exp(-x*V[10]))/(exp(-x*U[10])-exp(-x*V[10]))}
    su <- fun(theta1)
    theta2 <- n/(n/theta1 + su)
    print(round(c(theta2),9))
    if(i == max.it) break
    i <- i + 1    
  }
  return(theta2)
}

mle2 <- EM(U,V,max.it=10000,eps=1e-5)



## -----------------------------------------------------------------------------
iris[FALSE,]

iris[ , FALSE] # or iris[FALSE]

iris[FALSE, FALSE] # or just data.frame()


## -----------------------------------------------------------------------------
scale01 <- function(x) {
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
data.frame(lapply(freeny, function(x) if (is.numeric(x)) scale01(x) else x))

## -----------------------------------------------------------------------------
vapply(attitude, sd, numeric(1))

## -----------------------------------------------------------------------------
vapply(freeny[vapply(freeny, is.numeric, logical(1))], sd, numeric(1))


## ----eval=FALSE---------------------------------------------------------------
#  library(Rcpp)
#  #// This is the rw_MetropolisC.cpp
#  #include <Rcpp.h>
#  #using namespace Rcpp;
#  #// [[Rcpp::export]]
#  cppFunction('NumericMatrix gibbsC(int N, double mu1, double mu2, double sigma1, double sigma2, double pho) {
#    NumericMatrix mat(N, 2);
#    double s1 = sqrt(1-pho * pho) * sigma1, s2 = sqrt(1-pho * pho) * sigma2;
#    double x = 0, y = 0;
#    for(int i = 0; i < N; i++) {
#      double m1 = mu1 + pho * (y - mu2) * sigma1 / sigma2;
#      x = rnorm(1, m1, s1)[0];
#      mat(i, 0) = x;
#  
#      double m2 = mu2 + pho * (x - mu1) * sigma2 / sigma1;
#      y = rnorm(1, m2, s2)[0];
#      mat(i, 1) = y;
#    }
#    return(mat);
#  }')

## ----eval=FALSE---------------------------------------------------------------
#  library(Rcpp)
#  library(microbenchmark)
#  set.seed(123)
#  
#  gibbsR <- function(N, mu1, mu2, sigma1, sigma2, pho) {
#    mat <- matrix(nrow = N, ncol = 2)
#    x <- 0
#    y <- 0
#    s1 = sqrt(1-pho * pho) * sigma1
#    s2 = sqrt(1-pho * pho) * sigma2
#    for (i in 1:N) {
#      m1 = mu1 + pho * (y - mu2) * sigma1 / sigma2;
#      x = rnorm(1, m1, s1);
#      mat[i, 1] = x;
#  
#      m2 = mu2 + pho * (x - mu1) * sigma2 / sigma1;
#      y = rnorm(1, m2, s2);
#      mat[i, 2] = y;
#    }
#    mat
#  }
#  
#  gibbR=gibbsR(2000,0,0,1,1,0.9)
#  gibbC=gibbsC(2000,0,0,1,1,0.9)
#  
#  par(mfrow = c(1, 3))
#  qqplot(gibbC,gibbR)
#  qqplot(gibbC[,1],gibbR[,1])
#  qqplot(gibbC[,2],gibbR[,2])

## ----eval=FALSE---------------------------------------------------------------
#  
#  ts <- microbenchmark(gibbR=gibbsR(2000,0,0,1,1,0.9), gibbC=gibbsC(2000,0,0,1,1,0.9))
#  knitr::kable(summary(ts)[,c(1,3,5,6)])
#  

