# ENSEMBLE DIAGNOSTICS
library(e1071)
library(NeuralNetTools)
library(nnet)
library(rpart)
library(randomForest)
library(irr)
library(RSADBE)

setwd("C:/Users/tprabhan/Documents/My_Books/ELwR/R_Programs/Chapter_08/SRC")

load("../Data/GC2.RData")
table(GC2$good_bad)
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(GC2),replace = TRUE,prob = c(0.7,0.3))
head(Train_Test)
GC2_Train <- GC2[Train_Test=="Train",]
GC2_TestX <- within(GC2[Train_Test=="Test",],rm(good_bad))
GC2_TestY <- GC2[Train_Test=="Test","good_bad"]
GC2_TestY_numeric <- as.numeric(GC2_TestY)
GC2_Formula <- as.formula("good_bad~.")
p <- ncol(GC2_TestX)
ntr <- nrow(GC2_Train) 
nte <- nrow(GC2_TestX) 

# Logistic Regression
LR_fit <- glm(GC2_Formula,data=GC2_Train,family = binomial())
LR_Predict_Train <- predict(LR_fit,newdata=GC2_Train,type="response")
LR_Predict_Train <- as.factor(ifelse(LR_Predict_Train>0.5,"good","bad"))
LR_Accuracy_Train <- sum(LR_Predict_Train==GC2_Train$good_bad)/ntr
LR_Accuracy_Train
LR_Predict_Test <- predict(LR_fit,newdata=GC2_TestX,type="response")
LR_Predict_Test_Bin <- ifelse(LR_Predict_Test>0.5,2,1)
LR_Accuracy_Test <- sum(LR_Predict_Test_Bin==GC2_TestY_numeric)/nte
LR_Accuracy_Test

# Naive Bayes
NB_fit <- naiveBayes(GC2_Formula,data=GC2_Train)
NB_Predict_Train <- predict(NB_fit,newdata=GC2_Train)
NB_Accuracy_Train <- sum(NB_Predict_Train==GC2_Train$good_bad)/ntr
NB_Accuracy_Train
NB_Predict_Test <- predict(NB_fit,newdata=GC2_TestX)
NB_Accuracy_Test <- sum(NB_Predict_Test==GC2_TestY)/nte
NB_Accuracy_Test

# Decision Tree
CT_fit <- rpart(GC2_Formula,data=GC2_Train)
CT_Predict_Train <- predict(CT_fit,newdata=GC2_Train,type="class")
CT_Accuracy_Train <- sum(CT_Predict_Train==GC2_Train$good_bad)/ntr
CT_Accuracy_Train
CT_Predict_Test <- predict(CT_fit,newdata=GC2_TestX,type="class")
CT_Accuracy_Test <- sum(CT_Predict_Test==GC2_TestY)/nte
CT_Accuracy_Test

# Support Vector Machine
SVM_fit <- svm(GC2_Formula,data=GC2_Train)
SVM_Predict_Train <- predict(SVM_fit,newdata=GC2_Train,type="class")
SVM_Accuracy_Train <- sum(SVM_Predict_Train==GC2_Train$good_bad)/ntr
SVM_Accuracy_Train
SVM_Predict_Test <- predict(SVM_fit,newdata=GC2_TestX,type="class")
SVM_Accuracy_Test <- sum(SVM_Predict_Test==GC2_TestY)/nte
SVM_Accuracy_Test



## Ensemble Diversity

## Numeric Example
DN <- read.csv("../Data/Diverse_Numeric.csv")
windows(height=100,width=100)
plot(NULL,xlim=c(5,70),ylim=c(0,7),yaxt='n',xlab="X-values",ylab="")
points(DN[1,2:6],rep(1,5),pch=c(19,1,1,1,0),cex=2)
points(DN[2,2:6],rep(2,5),pch=c(19,1,1,1,0),cex=2)
points(DN[3,2:6],rep(3,5),pch=c(19,1,1,1,0),cex=2)
points(DN[4,2:6],rep(4,5),pch=c(19,1,1,1,0),cex=2)
points(DN[5,2:6],rep(5,5),pch=c(19,1,1,1,0),cex=2)
points(DN[6,2:6],rep(6,5),pch=c(19,1,1,1,0),cex=2)
legend(x=45,y=7,c("Actual","Model","Ensemble"),pch=c(19,1,0))
axis(2,at=1:6,labels=paste("Case",1:6),las=1)


# Drop the Geese Pair
GP <- function(Pred_Matrix) {
  L <- ncol(Pred_Matrix) # Number of classifiers
  N <- nrow(Pred_Matrix) # Number of observations
  GP_Matrix <- matrix(TRUE,nrow=L,ncol=L)
  for(i in 1:(L-1)){
    for(j in (i+1):L){
      GP_Matrix[i,j] <- ifelse(sum(Pred_Matrix[,i]==Pred_Matrix[,j])==N,
                             TRUE,FALSE)
      GP_Matrix[j,i] <- GP_Matrix[i,j]
    }
  }
  return(GP_Matrix)
  }

# A brief peek in CART_Dummy dataset
data(CART_Dummy)
CART_Dummy$Y <- as.factor(CART_Dummy$Y)
attach(CART_Dummy)
windows(height=100,width=200)
par(mfrow=c(1,2))
plot(c(0,12),c(0,10),type="n",xlab="X1",ylab="X2")
points(X1[Y==0],X2[Y==0],pch=15,col="red")
points(X1[Y==1],X2[Y==1],pch=19,col="green")
title(main="A Difficult Classification Problem")
plot(c(0,12),c(0,10),type="n",xlab="X1",ylab="X2")
points(X1[Y==0],X2[Y==0],pch=15,col="red")
points(X1[Y==1],X2[Y==1],pch=19,col="green")
segments(x0=c(0,0,6,6),y0=c(3.75,6.25,2.25,5),
         x1=c(6,6,12,12),y1=c(3.75,6.25,2.25,5),lwd=2)
abline(v=6,lwd=2)
title(main="Looks a Solvable Problem Under Partitions")


CD <- CART_Dummy 
CD$Y <- as.factor(CD$Y)
set.seed(1234567)
CD_RF <- randomForest(Y~.,data=CD,ntree=500)
CD_RF_Predict <- predict(CD_RF,newdata=CD,
                         type="class",predict.all=TRUE)
CD_RF_Predict_Matrix <- CD_RF_Predict$individual
CD_GP <- GP(CD_RF_Predict_Matrix)
CD_GP[1:8,1:8]
rowSums(CD_GP)
which(rowSums(CD_GP)>1)
which(CD_GP[21,]==TRUE)
which(CD_GP[42,]==TRUE)
which(CD_GP[176,]==TRUE)
which(CD_GP[206,]==TRUE)
which(CD_GP[221,]==TRUE)
which(CD_GP[365,]==TRUE)
which(CD_GP[385,]==TRUE)

# Building the random forest
set.seed(12345)
GC2_RF3 <- randomForest(GC2_Formula,data=GC2_Train,mtry=10,
                        parms = list(split="information",
                                     loss=matrix(c(0,1,1000,0),byrow = TRUE,nrow=2)),
                        ntree=1000)
GC2_RF_Train_Predict <- predict(GC2_RF3,newdata=GC2_Train,
                                type="class",predict.all=TRUE)
GC2_RF_Train_Predict_Matrix <- GC2_RF_Train_Predict$individual


GC2_RF_Train_Prob_Matrix <- GC2_RF_Train_Prob$individual
GC2_GP <- GP(GC2_RF_Train_Predict_Matrix)
rowSums(GC2_GP)
which(rowSums(GC2_GP)>1)


# Oracle Output
Oracle <- function(PM,Actual){
  # PM = Prediction Matrix, Actual = the true Y's
  OM <- matrix(0,nrow=nrow(PM),ncol=ncol(PM))
  for(i in 1:ncol(OM)) {
    OM[,i] <- as.numeric(PM[,i]==Actual)
  }
  return(OM)
}
GC_Oracle <- Oracle(PM=GC2_RF_Train_Predict$individual,
                    Actual=GC2_Train$good_bad)
colSums(GC_Oracle)/nrow(GC_Oracle)




# Diversity Measures
# Disagreement Measure
DM <- function(prediction1,prediction2){
  tp <- table(prediction1,prediction2)
  Diss <- (tp[1,2]+tp[2,1])/length(prediction1)
  return(Diss)
}
DM(LR_Predict_Train,NB_Predict_Train)
DM(LR_Predict_Train,CT_Predict_Train)
DM(LR_Predict_Train,SVM_Predict_Train)
DM(NB_Predict_Train,CT_Predict_Train)
DM(NB_Predict_Train,SVM_Predict_Train)
DM(CT_Predict_Train,SVM_Predict_Train)

# Q-statistic 
Yule <- function(prediction1,prediction2){
  tp <- table(prediction1,prediction2)
  Yu <- (tp[1,1]*tp[2,2]-tp[1,2]*tp[2,1])/(tp[1,1]*tp[2,2]+tp[1,2]*tp[2,1])
  return(Yu)
}
Yule(LR_Predict_Train,NB_Predict_Train)
Yule(LR_Predict_Train,CT_Predict_Train)
Yule(LR_Predict_Train,SVM_Predict_Train)
Yule(NB_Predict_Train,CT_Predict_Train)
Yule(NB_Predict_Train,SVM_Predict_Train)
Yule(CT_Predict_Train,SVM_Predict_Train)

1-DM(LR_Predict_Train,NB_Predict_Train)
1-DM(LR_Predict_Train,CT_Predict_Train)
1-DM(LR_Predict_Train,SVM_Predict_Train)
1-DM(NB_Predict_Train,CT_Predict_Train)
1-DM(NB_Predict_Train,SVM_Predict_Train)
1-DM(CT_Predict_Train,SVM_Predict_Train)


# Correlation coefficient 
# Sneath and Sokal, 1973
SS_Cor <- function(prediction1, prediction2){
  tp <- table(prediction1,prediction2)
  a <- tp[1,1]; b <- tp[2,1]; c <- tp[1,2]; d <- tp[2,2]
  SS <- (a*d-b*c)/sqrt(exp(log(a+b)+log(a+c)+log(c+d)+log(b+d)))
  return(SS)
}
SS_Cor(LR_Predict_Train,NB_Predict_Train)
SS_Cor(LR_Predict_Train,CT_Predict_Train)
SS_Cor(LR_Predict_Train,SVM_Predict_Train)
SS_Cor(NB_Predict_Train,CT_Predict_Train)
SS_Cor(NB_Predict_Train,SVM_Predict_Train)
SS_Cor(CT_Predict_Train,SVM_Predict_Train)

# Kappa-statistic 
# Cohen's Statistic
Kappa <- function(prediction1, prediction2){
  tp <- table(prediction1,prediction2)
  a <- tp[1,1]; b <- tp[2,1]; c <- tp[1,2]; d <- tp[2,2]
  n <- length(prediction1)
  theta1 <- (a+d)/n
  theta2 <- (((a+b)*(a+c))+((c+d)*(b+d)))/n^2
  kappa <- (theta1-theta2)/(1-theta2)
  return(kappa)
}
Kappa(LR_Predict_Train,NB_Predict_Train)
Kappa(LR_Predict_Train,CT_Predict_Train)
Kappa(LR_Predict_Train,SVM_Predict_Train)
Kappa(NB_Predict_Train,CT_Predict_Train)
Kappa(NB_Predict_Train,SVM_Predict_Train)
Kappa(CT_Predict_Train,SVM_Predict_Train)


# Double-fault Measure
Double_Fault <- function(prediction1,prediction2,actual){
  DF <- sum((prediction1!=actual)*(prediction2!=actual))/length(actual)
  return(DF)
}
Double_Fault(LR_Predict_Train,NB_Predict_Train,GC2_Train$good_bad)
Double_Fault(LR_Predict_Train,CT_Predict_Train,GC2_Train$good_bad)
Double_Fault(LR_Predict_Train,SVM_Predict_Train,GC2_Train$good_bad)
Double_Fault(NB_Predict_Train,CT_Predict_Train,GC2_Train$good_bad)
Double_Fault(NB_Predict_Train,SVM_Predict_Train,GC2_Train$good_bad)
Double_Fault(CT_Predict_Train,SVM_Predict_Train,GC2_Train$good_bad)


# Use IRR measures such as bhapkar, kappa2, kendall, 
bhapkar(GC_Train_Fit[,1:2])
bhapkar(GC_Train_Fit[,c(1,3)])
bhapkar(GC_Train_Fit[,c(1,4)])
bhapkar(GC_Train_Fit[,c(2,3)])
bhapkar(GC_Train_Fit[,c(2,4)])
bhapkar(GC_Train_Fit[,c(3,4)])
kappa2(GC_Train_Fit[,1:2])
kappa2(GC_Train_Fit[,c(1,3)])
kappa2(GC_Train_Fit[,c(1,4)])
kappa2(GC_Train_Fit[,c(2,3)])
kappa2(GC_Train_Fit[,c(2,4)])
kappa2(GC_Train_Fit[,c(3,4)])
kendall(GC_Train_Fit[,1:2])
kendall(GC_Train_Fit[,c(1,3)])
kendall(GC_Train_Fit[,c(1,4)])
kendall(GC_Train_Fit[,c(2,3)])
kendall(GC_Train_Fit[,c(2,4)])
kendall(GC_Train_Fit[,c(3,4)])


########## Interrater agreement 
# Illustrate functions such as agree, meancor,meanrho, and rater.bias
GC_Train_Fit <- cbind(LR_Predict_Train,NB_Predict_Train,CT_Predict_Train,
                      SVM_Predict_Train)
agree(GC_Train_Fit)


agree(GC2_RF_Train_Predict$individual[,1:5])
agree(GC2_RF_Train_Predict$individual[,1:10])
agree(GC2_RF_Train_Predict$individual)

kendall(GC_Train_Fit)

kendall(GC2_RF_Train_Predict$individual[,1:5])
kendall(GC2_RF_Train_Predict$individual[,1:10])
kendall(GC2_RF_Train_Predict$individual)


# Entropy Measure
# Page 250 of Kuncheva (2014)
Entropy_Measure <- function(OM){
  # OM = Oracle Matrix
  N <- nrow(OM); L <- ncol(OM)
  E <- 0
  for(i in 1:N){
    E <- E+min(sum(OM[i,]),L-sum(OM[i,]))
  }
  E <- 2*E/(N*(L-1))
  return(E)
}
Entropy_Measure(GC_Oracle)

# Kohavi-Wolpert variance 
# Using the predicted probability
KW <- function(Prob){
  N <- nrow(Prob)
  kw <- mean(1-Prob[,1]^2-Prob[,2]^2)/2
  return(kw)
}
GC2_RF_Train_Predict_Prob <- predict(GC2_RF3,newdata=GC2_Train,
                                type="prob",predict.all=TRUE)
GC2_RF_Train_Prob <- GC2_RF_Train_Predict_Prob$aggregate
KW(GC2_RF_Train_Prob)

# Using the Oracle matrix
KW_OM <- function(OM){
  # OM is the oracle matrix
  N <- nrow(OM); L <- ncol(OM)
  kw <- 0
  for(i in 1:N){
    lz <- sum(OM[i,])
    kw <- kw + lz*(L-lz)
  }
  kw <- kw/(N*L^2)
  return(kw)
}
KW_OM(GC_Oracle)

# Disagreement Measure OVerall on Oracle Matrix
DMO <- function(OM){
  # OM is the oracle matrix
  N <- nrow(OM); L <- ncol(OM)
  dmo <- 0
  for(i in 1:L){
    for(j in c(c(1:L)[c(1:L)!=i])){
      dmo <- dmo + sum((OM[,i]-OM[,j])^2)/N
    }
  }
  dmo <- dmo/(L*(L-1))
  return(dmo)
}

DM_GC <- DMO(OM=GC_Oracle)
DM_GC
KW_OM(GC_Oracle)
DM_GC*999/2000

# Average Ensemble Accuracy 
Avg_Ensemble_Acc <- function(Oracle){
  return(mean(colSums(GC_Oracle)/nrow(GC_Oracle)))
}
Avg_Ensemble_Acc(GC_Oracle)

# Measurement of Interrater Agreement
Kappa <- function(Oracle){
  pbar <- Avg_Ensemble_Acc(Oracle)
  AvgL <- 0
  N <- nrow(Oracle); L <- ncol(Oracle)
  for(i in 1:N){
    lz <- sum(Oracle[i,])
    AvgL <- AvgL + lz*(L-lz)
  }
  Avgl <- AvgL/L
  kappa <- 1-Avgl/(N*(L-1)*pbar*(1-pbar))
  return(kappa)
}
Kappa(GC_Oracle)
1-DM_GC/(2*Avg_Ensemble_Acc(GC_Oracle)*(1-Avg_Ensemble_Acc(GC_Oracle)))

# Entropy (C&C's) 

# Entropy (S&K's) 

# Difficulty 

# Generalized diversity 

# Coincident failure


## Ensemble Pruning


# Hierarchical Clustering for Grouping Classifiers



