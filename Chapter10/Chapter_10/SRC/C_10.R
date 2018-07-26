library(survival)
library(flexsurv)
library(rpart)
library(randomForestSRC)
library(gbm)


#setwd("C:/Users/tprabhan/Documents/My_Books/ELwR/R_Programs/Chapter_03/SRC")
setwd("/home/pranathi/Documents/ELwR/R_Programs/Chapter_10/SRC")

windows(height=100,width=100)
# Survival Concepts, hazard rate, cumulative hazard function, survival function
par(mfrow=c(3,3))
# Exponential Distribution
Time <- seq(0,100,1)
lambda <- 1/20
expdens <- dexp(Time,rate=lambda)
expsurv <- 1-pexp(Time,rate=lambda)
exphaz <- expdens/expsurv
expcumhaz <- -log(expsurv)
plot(Time,exphaz,"l",xlab="Time",ylab="Hazard Rate",ylim=c(0,0.1))
plot(Time,expcumhaz,"l",xlab="Time",ylab="Cumulative Hazard Function")
mtext("Exponential Distribution")
plot(Time,expsurv,"l",xlab="Time",ylab="Survival Function")

# Gamma Distribution
lambda <- 1/10; k <- 2
gammadens <- dgamma(Time,rate=lambda,shape=k)
gammasurv <- 1-pgamma(Time,rate=lambda,shape=k)
gammahaz <- gammadens/gammasurv
gammacumhaz <- -log(gammahaz)
plot(Time,gammahaz,"l",xlab="Time",ylab="Hazard Rate")
plot(Time,gammacumhaz,"l",xlab="Time",ylab="Cumulative Hazard Function")
mtext("Gamma Distribution")
plot(Time,gammasurv,"l",xlab="Time",ylab="Survival Function")

# Weibull Distribution
lambda <- 25; k <- 2
Weibulldens <- dweibull(Time,scale=lambda,shape=k)
Weibullsurv <- 1-pweibull(Time,scale=lambda,shape=k)
Weibullhaz <- Weibulldens/Weibullsurv
Weibullcumhaz <- -log(Weibullsurv)
plot(Time,Weibullhaz,"l",xlab="Time",ylab="Hazard Rate")
plot(Time,Weibullcumhaz,"l",xlab="Time",ylab="Cumulative Hazard Function")
mtext("Weibull Distribution")
plot(Time,Weibullsurv,"l",xlab="Time",ylab="Survival Function")

pbc <- survival::pbc
Surv(pbc$time,pbc$status==2)
pbc_exp <- flexsurvreg(Surv(time,status==2)~1,data=pbc,dist="exponential")
pbc_exp
windows(height=100,width=100)
plot(pbc_exp,ylim=c(0,1),col="black")
pbc_gamma <- flexsurvreg(Surv(time,status==2)~1,data=pbc,dist="gamma")
pbc_gamma
plot(pbc_gamma,col="blue",add=TRUE)
pbc_Weibull <- flexsurvreg(Surv(time,status==2)~1,data=pbc,dist="weibull")
pbc_Weibull
plot(pbc_Weibull,col="orange",add=TRUE)
legend(3000,1,c("Exponential","Gamma","Weibull"),
       col=c("black","blue","orange"),merge=TRUE,lty=2)



# Nonparametric Inference
# Kaplan-Meier, Nelson-Aalen estimators, and log-rank test 
pbc <- survival::pbc
# Kaplan-Meier Estimator
pbc_sf <- survfit(Surv(time,status==2)~1,pbc)
pbc_sf
median(pbc$time)
median(pbc$time[pbc$status==2])
median(pbc$time[pbc$status!=2])
summary(pbc_sf,times=as.numeric(quantile(pbc$time,seq(0,1,0.1))))
plot(pbc_sf,xlab="Time",ylab="Survival Function Confidence Bands")
# Nelson-Aalen Estimator
pbc_na <- basehaz(coxph(Surv(time,status==2)~1,pbc))
pbc_na
plot(pbc_na$time,pbc_na$hazard,"l",xlab="Time",ylab="Cumulative Hazard Function")
str(exp(-pbc_na$hazard))
str(summary(pbc_sf,times=pbc_na$time)$surv)

# Log-rank test
plot(survfit(Surv(time,status==2)~sex,pbc),conf.int=TRUE,xlab="Time",
     ylab="Survival Probability",col=c("red","blue"))
survdiff(Surv(time,status==2)~sex,pbc)
round(exp(-pbc_na$hazard),4)==round(summary(pbc_sf,times=pbc_na$time)$surv,4)

# Fitting regression model, parametric and Cox PH model
pbc_Exp <- survreg(Surv(time,status==2)~trt + age + sex + ascites + 
                     hepato + spiders + edema + bili + chol + albumin + 
                     copper + alk.phos + ast + trig + platelet + 
                     protime + stage,
                   dist="exponential",
                   pbc)
pbc_Exp_summary <- summary(pbc_Exp)
pbc_Exp_summary
round(pbc_Exp_summary$table[,4],4)
AIC(pbc_exp)
pbc_Exp_eff <- step(pbc_Exp)
pbc_Exp_eff_summary <- summary(pbc_Exp_eff)
round(pbc_Exp_eff_summary$table[,4],4)
AIC(pbc_Exp_eff)
#plot(pbc_Exp)
pbc2 <- na.omit(pbc)
pbc_coxph <- coxph(Surv(time,status==2)~trt + age + sex + ascites + 
                     hepato + spiders + edema + bili + chol + albumin + 
                     copper + alk.phos + ast + trig + platelet + 
                     protime + stage,
                   pbc2)
pbc_coxph_summary <- summary(pbc_coxph)
pbc_coxph_summary
round(pbc_coxph_summary$coefficients[,5],4)
AIC(pbc_coxph)
pbc_coxph_eff <- step(pbc_coxph)
pbc_coxph_eff_summary <- summary(pbc_coxph_eff)
pbc_coxph_eff_summary
round(pbc_coxph_eff_summary$coefficients[,5],4)
AIC(pbc_coxph_eff)


# Survival Tree
pbc_stree <- rpart(Surv(time,status==2)~trt + age + sex + ascites + 
                     hepato + spiders + edema + bili + chol + albumin + 
                     copper + alk.phos + ast + trig + platelet + 
                     protime + stage,
                   pbc)
pbc_stree
pbc_stree$cptable
pbc_stree$variable.importance
windows(height=100,width=60)
plot(pbc_stree,uniform = TRUE)
text(pbc_stree,use.n=TRUE)



# Ensemble methods for survival data
pbc_rf <- rfsrc(Surv(time,status==2)~trt + age + sex + ascites + hepato + 
                  spiders + edema + bili + chol + albumin + copper + 
                  alk.phos + ast + trig + platelet + protime + stage,
                   ntree=500,tree.err = TRUE,
                   pbc)
pbc_rf$splitrule
pbc_rf$nodesize
pbc_rf$mtry
vimp(pbc_rf)$importance
var.select(pbc_rf, method = "vh.vimp", nrep = 50)
pdf("RandomForestSurvival_PBC.pdf")
plot(pbc_rf,plots.one.page = TRUE)
dev.off()

# Gradient Boosting for Survival Data
pbc_gbm <- gbm(Surv(time,status==2)~trt + age + sex + ascites + 
                 hepato + spiders + edema + bili + chol + albumin + 
                 copper + alk.phos + ast + trig + platelet + 
                 protime + stage,
               data=pbc)
  