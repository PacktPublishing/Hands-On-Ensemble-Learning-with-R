library(boot)
library(survival)
library(RSADBE)
library(car)
library(ACSWR)
install.packages("ACSWR")
library(pseudo)
install.packages("pseudo")
install.packages("gee")
library(gee)
library(mvtnorm)

setwd("C:/")

# The Jackknife

# Simple examples of mean, variance, etc

# Simulating observations from Weibull distribution
set.seed(123)
sr <- rweibull(1000,0.5,15)
mean(sr); sd(sr); var(sr)
# Calculating the pseudovalues for the mean
pv_mean <- NULL; n <- length(sr)
for(i in 1:n)
  pv_mean[i] <- sum(sr)- (n-1)*mean(sr[-i])
head(sr,20)
head(pv_mean,20)
mean(pv_mean); sd(pv_mean)
# Calculating the pseudovalues for the variance
pv_var <- NULL
pseudo_var <- function(x,i){
  n = length(x)
  psv <- (n/(n-2))*(x[i]-mean(x))^2-(1/(n-1)*(n-2))*sum(x-mean(x))^2
  return(psv)
}
pv_var <- NULL
for(i in 1:n)
  pv_var[i] <- pseudo_var(sr,i)
head(pv_var)
var(sr); mean(pv_var)
sd(pv_var)


# Survival modeling of the Andersen and Klein approach of jackknife, 
# pseudo-observations, and GEE
data(pbc)
time_pseudo <- pseudomean(time=pbc$time,event=pbc$status==2)
pbc_gee <- gee(time_pseudo ~ trt + age + sex + ascites + hepato +
                 spiders + edema + bili + chol + albumin + copper + 
                 alk.phos + ast + trig + platelet + protime + stage,
               id=1:nrow(pbc), family="gaussian",
               data=pbc)
summary(pbc_gee)
install.packages("gee")


# Standard Error of Correlation Coefficient
LS <- read.csv("../Data/Law_School.csv",header=TRUE)
LS
windows(height=100,width=100)
layout(matrix(c(1,2,3,3),byrow=TRUE, nrow=2))
hist(LS$LSAT,xlab="LSAT",main="Histogram of LSAT")
hist(LS$GPA,xlab="GPA",main="Histogram of GPA")
plot(LS[,2:3],main="Scatter plot between LSAT and GPA")

n <- nrow(LS)
B <- 3200
TB <- c(25,50,100,200,400,800,1600,3200)
corrB <- NULL
seB <- NULL
tcorr <- NULL
myseed <- 54321
for(i in 1:B){
  myseed <- myseed+1
  set.seed(myseed)
  tcorr[i] <- as.numeric(cor(LS[sample(1:n,n,replace=TRUE),2:3])[1,2])
}
for(j in 2:B){
  corrB[j] <- mean(tcorr[1:j])
  seB[j] <- sd(tcorr[1:j])
  }
round(corrB[TB],3)
round(seB[TB],3)
plot.ts(seB,xlab="Number of Bootstrap Samples",
        ylab="Bootstrap Standard Error of Correlation")
for(i in 1:length(TB)) print(quantile(tcorr[1:TB[i]],c(0.025,0.975)))

# Parametric Bootstrap for the Standard Error 
LS_mean <- colMeans(LS[,2:3])
LS_var<- var(LS[,2:3])
LS_mean; LS_var
B <- 3200
TB <- c(25,50,100,200,400,800,1600,3200)
ptcorr <- NULL
ptcorrB <- NULL
pseB <- NULL
myseed <- 54321
for(i in 1:B){
  myseed <- myseed+1
  set.seed(myseed)
  temp <- rmvnorm(n,LS_mean,LS_var)
  ptcorr[i] <- as.numeric(cor(temp)[1,2])
}
for(j in 2:B){
  ptcorrB[j] <- mean(ptcorr[1:j])
  pseB[j] <- sd(ptcorr[1:j])
}
round(ptcorrB[TB],3)
round(pseB[TB],3)
plot.ts(pseB,xlab="Number of Bootstrap Samples",
        ylab="Parametric Bootstrap Standard Error of Correlation")
for(i in 1:length(TB)) print(quantile(ptcorr[1:TB[i]],c(0.025,0.975)))


# Eigen Values and Standard Error
# Source: http://www1.maths.leeds.ac.uk/~charles/mva-data/openclosedbook.dat
OC <- read.csv("../Data/OpenClose.csv")
OC_xbar <- colMeans(OC)
pairs(OC)
OC_xbar
OC_Cov <- cov(OC)
OC_Cov
OC_Cor <- cor(OC)
OC_Cor

OC_eigen <- eigen(OC_Cov)
OC_eigen$values
OC_eigen$vectors
OC_eigen$values/sum(OC_eigen$values)


thetaB <- NULL; sethetaB <- NULL
B <- 500
n <- nrow(OC)
myseed <- 54321
for(i in 1:B){
  myseed <- myseed+1
  set.seed(myseed)
  OCt <- OC[sample(1:n,n,replace=TRUE),]
  OCt_eigen <- eigen(cov(OCt))
  thetaB[i] <- max(OCt_eigen$values)/sum(OCt_eigen$values)
}
for(j in 2:B){
  thetaB[j] <- mean(thetaB[1:j])
  sethetaB[j] <- sd(thetaB[1:j])
}
plot.ts(sethetaB,xlab="Number of Bootstrap Samples",
        ylab="Bootstrap Standard Error for First Principal Component")
TB <- seq(50,500,50)
for(i in 1:length(TB)) print(quantile(thetaB[1:TB[i]],c(0.025,0.975)))


# The boot Package
corx <- function(data,i) cor(data[i,1],data[i,2])
corboot <- boot(data=LS[,2:3],statistic=corx,R=200,stype="i")
corboot
corboot$t
confint(corboot)

Eigen_fn <- function(data,i)  {
 eig <- eigen(cov(data[i,]))
 val <- max(eig$values)/sum(eig$values)
 val
}
eigenboot <- boot(data=OC,statistic = Eigen_fn,R=200,stype = "i")
eigenboot
confint(eigenboot)



# Bootstrap and Testing Hypotheses
t2 <- function(data,i) {
  p <- t.test(data[i,1],data[i,2],var.equal=TRUE)$statistic
  p
}
data(galton)
gt <- boot(galton,t2,R=100)
gt
confint(gt)
t.test(galton[,1],galton[,2],var.equal=TRUE)


v2 <- function(data,i) {
  v <- var.test(data[i,1],data[i,2])$statistic
  v
}
gv <- boot(galton,v2,R=100)
gv
confint(gv)
var.test(galton[,1],galton[,2])


# Bootstrap for Linear Regression Model
data(usc)
usc_Formula <- as.formula("R~.")
usc_lm <- lm(usc_Formula,usc)
summary(usc_lm)

f <- function(obj) summary(obj)$adj.r.squared
usc_boot <- Boot(usc_lm,f=f,R=100)
summary(usc_boot)
confint(usc_boot)


# Bootstrapping Survival Models
Med_Surv <- function(data){
  s2 <- survfit(Surv(time,status==2)~1,data=data)
  s2s <- summary(s2)
  s2median <- s2s$time[which.min(s2s$surv>0.5)]
  s2median
}
pbc2 <- pbc[,2:3]
pbc_median_boot <- censboot(data=pbc2,statistic=Med_Surv,R=100)
pbc_median_boot
pbc_median_boot$t
confint(pbc_median_boot)


Mean_Surv <- function(data,time){
  s2 <- survfit(Surv(time,status==2)~1,data=data)
  smean <- as.numeric(
    survival:::survmean(s2,rmean=time)[[1]]["*rmean"])
  smean
}
censboot(data=pbc2,time=2000,statistic=Mean_Surv,R=100)


# Bootstrapping Time Series Models
Var.fun <- function(ts) {
  ar.fit <- ar(ts, order.max = 25)
  ar.fit$var
}
AP_Boot <- tsboot(AirPassengers,Var.fun,R=999,l=20,sim="fixed")
AP_Boot
quantile(AP_Boot$t,c(0.025,0.975))



