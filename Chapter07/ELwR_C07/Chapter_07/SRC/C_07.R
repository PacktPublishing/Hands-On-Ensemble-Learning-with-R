### Understanding the Ensembling Theory
library(nnet)
library(rpart)
library(e1071)
library(randomForest)
library(caret)
library(gbm)

setwd("C:/Users/tprabhan/Documents/My_Books/ELwR/R_Programs/Chapter_07/SRC")
source("Utilities.R")

# Ensembling in Classification
# Illustrating the ensemble accuracy with same accuracy for each classifier
# Different p's and T's with p > 0.5
classifiers <- seq(9,45,2) # Number of classifiers 
accuracy <- seq(0.55,0.85,.05)
plot(0,type='n',xlim=range(classifiers),ylim=c(0.6,1),
     xlab="Number of Classifiers",ylab="Probability of Majority Voting")
for(i in 1:length(accuracy)){
  Prob_MV <- NULL
  for(j in 1:length(classifiers)){
    Prob_MV[j] <- sum(dbinom(floor(classifiers[j]/2+1):classifiers[j],
                             prob=accuracy[i],size=classifiers[j]))
  }
  points(classifiers,Prob_MV,col=i,"l")
  }
title("Classifiers with Accuracy Better Than Random Guess")
Prob_MV <- NULL
for(j in 1:length(classifiers)){
  Prob_MV[j] <- sum(dbinom(floor(classifiers[j]/2+1):classifiers[j],
                           prob=0.65,size=classifiers[j]))
}
Prob_MV

# When p < 0.5, ensemble accuracy goes to zero
classifiers <- seq(6,50,2)
accuracy <- seq(0.45,0.05,-0.05)
plot(0,type='n',xlim=range(classifiers),ylim=c(0,0.3),
     xlab="Number of Classifiers",ylab="Probability of Majority Voting")
for(i in 1:length(accuracy)){
  Prob_MV <- NULL
  for(j in 1:length(classifiers)){
    Prob_MV[j] <- sum(dbinom(floor(classifiers[j]/2+1):classifiers[j],
                             prob=accuracy[i],size=classifiers[j]))
  }
  points(classifiers,Prob_MV,col=i,"l")
  }
title("Classifiers with Accuracy Worse Than Random Guess")

classifiers <- seq(10,200,10)
Prob_MV <- NULL
for(j in 1:length(classifiers)){
  Prob_MV[j] <- sum(dbinom(floor(classifiers[j]/2+1):classifiers[j],
                           prob=0.4999,size=classifiers[j]))
}
Prob_MV

# When p = 0.5, no increase in accuracy, irrespective of T's
accuracy <- 0.5
classifiers <- seq(5,45,2)
Prob_MV <- NULL
for(j in 1:length(classifiers)){
  Prob_MV[j] <- sum(dbinom(floor(classifiers[j]/2+1):classifiers[j],
                           prob=accuracy,size=classifiers[j]))
  }
Prob_MV
classifiers <- seq(10,50,2)
Prob_MV <- NULL
for(j in 1:length(classifiers)){
  Prob_MV[j] <- (sum(dbinom(floor(classifiers[j]/2):classifiers[j],
                            prob=accuracy,size=classifiers[j]))+
                   sum(dbinom(floor(classifiers[j]/2+1):classifiers[j],
                              prob=accuracy,size=classifiers[j])))/2
  }
Prob_MV


# Different accuracies T's illustration
# For simplicity, we set the number of classifiers at odd number
# Each p_i's greater than 0.5
accuracy <- c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9)
NT <- length(accuracy) # Number of classifiers
APC <- expand.grid(rep(list(c(TRUE,FALSE)),NT)) # All possible combinations
head(APC)
Elements_Prob <- t(apply(APC,1,Get_Prob,Probability=accuracy))
head(Elements_Prob)
Events_Prob <- apply(Elements_Prob,1,prod)
Majority_Events <- (rowSums(APC)>NT/2)
sum(Events_Prob*Majority_Events)

accuracy <- c(0.7,0.7,0.7,0.9,0.9)
NT <- length(accuracy) # Number of classifiers
APC <- expand.grid(rep(list(c(TRUE,FALSE)),NT)) # All possible combinations
Elements_Prob <- t(apply(APC,1,Get_Prob,Probability=accuracy))
Events_Prob <- apply(Elements_Prob,1,prod)
Majority_Events <- (rowSums(APC)>NT/2)
sum(Events_Prob*Majority_Events)


# Each p_i's lesser than 0.5
accuracy <- 1-c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9)
NT <- length(accuracy) # Number of classifiers
APC <- expand.grid(rep(list(c(TRUE,FALSE)),NT)) # All possible combinations
head(APC)
Elements_Prob <- t(apply(APC,1,Get_Prob,Probability=accuracy))
head(Elements_Prob)
Events_Prob <- apply(Elements_Prob,1,prod)
Majority_Events <- (rowSums(APC)>NT/2)
sum(Events_Prob*Majority_Events)

# Mixture of p_i's, some > 0.5, and some < 0.5
Random_Accuracy <- function() {
  accuracy <- runif(9)
  NT <- length(accuracy) 
  APC <- expand.grid(rep(list(c(TRUE,FALSE)),NT)) 
  Elements_Prob <- t(apply(APC,1,Get_Prob,Probability=accuracy))
  Events_Prob <- apply(Elements_Prob,1,prod)
  Majority_Events <- (rowSums(APC)>NT/2)
  return(sum(Events_Prob*Majority_Events))
}
Random_Accuracy()
Random_Accuracy()
Random_Accuracy()
Random_Accuracy()
Random_Accuracy()
Random_Accuracy()
Random_Accuracy()
Random_Accuracy()
Random_Accuracy()
Random_Accuracy()



# Voting for Classification
load("../Data/GC2.RData")
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(GC2),replace = TRUE,prob = c(0.7,0.3))
GC2_Train <- GC2[Train_Test=="Train",]
GC2_TestX <- within(GC2[Train_Test=="Test",],rm(good_bad))
GC2_TestY <- GC2[Train_Test=="Test","good_bad"]
GC2_Formula <- as.formula("good_bad~.")


# RANDOM FOREST ANALYSIS
GC2_RF <- randomForest(GC2_Formula,data=GC2_Train,keep.inbag=TRUE,
                       ntree=500)

# New data voting
# The next line gives overall class prediction
GC2_RF_Test_Margin <- predict(GC2_RF,newdata = GC2_TestX,
                         type="class")
# The next line gives prediction at each tree level
GC2_RF_Test_Predict <- predict(GC2_RF,newdata=GC2_TestX,
                          type="class",predict.all=TRUE)
sum(GC2_RF_Test_Margin==GC2_RF_Test_Predict$aggregate)
head(GC2_RF_Test_Margin==GC2_RF_Test_Predict$aggregate)
head(cbind(GC2_RF_Test_Margin,GC2_RF_Test_Predict$aggregate))

# Majority Voting
View(GC2_RF_Test_Predict$individual) # Prediction at each tree
dim(GC2_RF_Test_Predict$individual)
Voting_Predict <- apply(GC2_RF_Test_Predict$individual,1,Row_Count_Max)
head(Voting_Predict);tail(Voting_Predict)
all(Voting_Predict==GC2_RF_Test_Predict$aggregate)
all(Voting_Predict==GC2_RF_Test_Margin)
sum(Voting_Predict==GC2_TestY)/313


# Analyzing Accuracy of Trees of the Fitted Forest
GC2_RF_Train_Predict <- predict(GC2_RF,newdata=GC2_Train[,-20],
                                type="class",predict.all=TRUE)
GC2_RF_Train_Predict2 <- predict(GC2_RF,newdata=GC2_Train[,-20],
                                type="prob",predict.all=TRUE)
head(GC2_RF_Train_Predict$individual[,c(1:5,496:500)])  
RF_Tree_Train_Accuracy <- NULL
for(i in 1:GC2_RF$ntree){
  RF_Tree_Train_Accuracy[i] <- sum(GC2_RF_Train_Predict$individual[,i]==
                                  GC2_Train$good_bad)/nrow(GC2_Train)
}
headtail(sort(RF_Tree_Train_Accuracy),10)

# Bagging ANALYSIS
GC2_Bagg <- randomForest(GC2_Formula,data=GC2_Train,keep.inbag=TRUE,
                         mtry=ncol(GC2_TestX),ntree=500)
GC2_Bagg_Test_Predict <- predict(GC2_Bagg,newdata=GC2_TestX,
                                type="class",predict.all=TRUE)
GC2_Bagg_Train_Predict <- predict(GC2_Bagg,newdata=GC2_Train[,-20],
                                type="class",predict.all=TRUE)

Bagg_Tree_Train_Accuracy <- NULL
for(i in 1:GC2_Bagg$ntree){
  Bagg_Tree_Train_Accuracy[i] <- sum(GC2_Bagg_Train_Predict$individual[,i]==
                                  GC2_Train$good_bad)/nrow(GC2_Train)
}
headtail(sort(Bagg_Tree_Train_Accuracy),10)

# Weighted Voting with Random Forest
RF_Weights <- RF_Tree_Train_Accuracy/sum(RF_Tree_Train_Accuracy)
Bagg_Weights <- Bagg_Tree_Train_Accuracy/sum(Bagg_Tree_Train_Accuracy)
RF_Weighted_Vote <- data.frame(matrix(0,nrow(GC2_TestX),ncol=3))
names(RF_Weighted_Vote) <- c("Good_Weight","Bad_Weight","Prediction")
for(i in 1:nrow(RF_Weighted_Vote)){
  RF_Weighted_Vote$Good_Weight[i] <- 
    sum((GC2_RF_Test_Predict$individual[i,]=="good")*RF_Weights)
  RF_Weighted_Vote$Bad_Weight[i] <- 
    sum((GC2_RF_Test_Predict$individual[i,]=="bad")*RF_Weights)
  RF_Weighted_Vote$Prediction[i] <- c("good","bad")[which.max(RF_Weighted_Vote[i,1:2])]
}
head(RF_Weighted_Vote,10)

# Weighted Voting with Bagging
Bagg_Weights <- Bagg_Tree_Train_Accuracy/sum(Bagg_Tree_Train_Accuracy)
Bagg_Weights <- Bagg_Tree_Train_Accuracy/sum(Bagg_Tree_Train_Accuracy)
Bagg_Weighted_Vote <- data.frame(matrix(0,nrow(GC2_TestX),ncol=3))
names(Bagg_Weighted_Vote) <- c("Good_Weight","Bad_Weight","Prediction")
for(i in 1:nrow(Bagg_Weighted_Vote)){
  Bagg_Weighted_Vote$Good_Weight[i] <- 
    sum((GC2_Bagg_Test_Predict$individual[i,]=="good")*Bagg_Weights)
  Bagg_Weighted_Vote$Bad_Weight[i] <- 
    sum((GC2_Bagg_Test_Predict$individual[i,]=="bad")*Bagg_Weights)
  Bagg_Weighted_Vote$Prediction[i] <- c("good","bad")[which.max(Bagg_Weighted_Vote[i,1:2])]
}
head(Bagg_Weighted_Vote,10)


# Averaging for Regression Problems
load("../Data/ht_imp_author.Rdata") # returns ht_imp object
load("../Data/htest_imp_author.Rdata") # returns htest_imp
names(ht_imp)[69] <- "SalePrice"
dim(ht_imp)
dim(htest_imp)
hf <- as.formula("SalePrice~.")
SP_lm <- lm(hf,data=ht_imp)
SP_rpart2 <- rpart(hf,data=ht_imp,maxdepth=2)
SP_rpart4 <- rpart(hf,data=ht_imp,maxdepth=4)
SP_rpart6 <- rpart(hf,data=ht_imp,maxdepth=6)
SP_rpart8 <- rpart(hf,data=ht_imp,maxdepth=8)
SP_nn2 <- nnet(hf,data=ht_imp,size=2,linout=TRUE)
SP_nn3 <- nnet(hf,data=ht_imp,size=3,linout=TRUE)
SP_nn4 <- nnet(hf,data=ht_imp,size=4,linout=TRUE)
SP_nn5 <- nnet(hf,data=ht_imp,size=5,linout=TRUE)
SP_svm <- svm(hf,data=ht_imp)

# Simple Averaging
SP_lm_pred <- predict(SP_lm,newdata=htest_imp)
SP_rpart2_pred <- predict(SP_rpart2,newdata=htest_imp)
SP_rpart4_pred <- predict(SP_rpart4,newdata=htest_imp)
SP_rpart6_pred <- predict(SP_rpart6,newdata=htest_imp)
SP_rpart8_pred <- predict(SP_rpart8,newdata=htest_imp)
SP_nn2_pred <- predict(SP_nn2,newdata=htest_imp)
SP_nn3_pred <- predict(SP_nn3,newdata=htest_imp)
SP_nn4_pred <- predict(SP_nn4,newdata=htest_imp)
SP_nn5_pred <- predict(SP_nn5,newdata=htest_imp)
SP_svm_pred <- predict(SP_svm,newdata=htest_imp)

windows(height=300,width=400)
par(mfrow=c(2,5))
plot.ts(SP_lm_pred,col=1)
plot.ts(SP_rpart2_pred,col=2)
plot.ts(SP_rpart4_pred,col=3)
plot.ts(SP_rpart6_pred,col=4)
plot.ts(SP_rpart8_pred,col=5)
plot.ts(SP_nn2_pred,col=6)
plot.ts(SP_nn3_pred,col=7)
plot.ts(SP_nn4_pred,col=8)
plot.ts(SP_nn5_pred,col=9)
plot.ts(SP_svm_pred,col=10)

Avg_Ensemble_Prediction <- rowMeans(cbind(SP_lm_pred,SP_rpart2_pred,SP_rpart4_pred,SP_rpart6_pred,
               SP_rpart8_pred,SP_nn4_pred,SP_nn5_pred,SP_svm_pred))
plot.ts(Avg_Ensemble_Prediction)


# Weighted Averaging
SP_lm_sigma <- mean(residuals(SP_lm)^2)
SP_rp2_sigma <- mean(residuals(SP_rpart2)^2)
SP_rp4_sigma <- mean(residuals(SP_rpart4)^2)
SP_rp6_sigma <- mean(residuals(SP_rpart6)^2)
SP_rp8_sigma <- mean(residuals(SP_rpart8)^2)
SP_nn4_sigma <- mean(residuals(SP_nn4)^2)
SP_nn5_sigma <- mean(residuals(SP_nn5)^2)
SP_svm_sigma <- mean(residuals(SP_svm)^2)

sigma_sum <- SP_lm_sigma + SP_rp2_sigma + SP_rp4_sigma +
  SP_rp6_sigma + SP_rp8_sigma + SP_nn4_sigma +
  SP_nn5_sigma + SP_svm_sigma 
sigma_sum

SP_lm_wts <- SP_lm_sigma/sigma_sum
SP_rp2_wts <- SP_rp2_sigma/sigma_sum
SP_rp4_wts <- SP_rp4_sigma/sigma_sum
SP_rp6_wts <- SP_rp6_sigma/sigma_sum
SP_rp8_wts <- SP_rp8_sigma/sigma_sum
SP_nn4_wts <- SP_nn4_sigma/sigma_sum
SP_nn5_wts <- SP_nn5_sigma/sigma_sum
SP_svm_wts <- SP_svm_sigma/sigma_sum

Weighted_Ensemble_Prediction <- rowMeans(cbind(SP_lm_wts*SP_lm_pred,
                                          SP_rp2_wts*SP_rpart2_pred,
                                          SP_rp4_wts*SP_rpart4_pred,
                                          SP_rp6_wts*SP_rpart6_pred,
                                          SP_rp8_wts*SP_rpart8_pred,
                                          SP_nn4_wts*SP_nn4_pred,
                                          SP_nn5_wts*SP_nn5_pred,
                                          SP_svm_wts*SP_svm_pred))
plot.ts(Weighted_Ensemble_Prediction)


# Stack Ensemble Examples
SP_lm_train <- predict(SP_lm,newdata=ht_imp)
SP_rpart2_train <- predict(SP_rpart2,newdata=ht_imp)
SP_rpart4_train <- predict(SP_rpart4,newdata=ht_imp)
SP_rpart6_train <- predict(SP_rpart6,newdata=ht_imp)
SP_rpart8_train <- predict(SP_rpart8,newdata=ht_imp)
SP_nn4_train <- predict(SP_nn4,newdata=ht_imp)
SP_nn5_train <- predict(SP_nn5,newdata=ht_imp)
SP_svm_train <- predict(SP_svm,newdata=ht_imp)

ht_imp2 <- cbind(ht_imp[,-69],SP_lm_train,SP_rpart2_train,SP_rpart4_train,
                          SP_rpart6_train,SP_rpart8_train,SP_nn4_train,SP_nn5_train,
                          SP_svm_train,ht_imp[,69])
names(ht_imp2)[77] <- "SalePrice"
SP_gbm <- gbm(hf,data=ht_imp2,distribution = "gaussian",n.trees=200)
headtail(predict(SP_gbm,n.trees=100),20)