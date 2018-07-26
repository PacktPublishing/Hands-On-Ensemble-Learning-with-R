### Ensemble Methods for Regression Data ###
library(missForest)
library(nnet)
library(NeuralNetTools)
library(rpart)
library(FactoMineR)
library(ClustOfVar)
library(plyr)
library(ipred)
library(gbm)
library(adabag)
library(missForest)
library(RSADBE)
library(caret)
library(caretEnsemble)


setwd("C:/Users/tprabhan/Documents/My_Books/ELwR/R_Programs/Chapter_09/SRC")
source("../../Chapter_04/SRC/Utilities.R")

# Housing Prices
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/

# Data Pre-processing
housing_train <- read.csv("../Data/Housing/train.csv",
                          row.names = 1,na.strings = "NA",
                          stringsAsFactors = TRUE)
housing_test <- read.csv("../Data/Housing/test.csv",
                          row.names = 1,na.strings = "NA",
                          stringsAsFactors = TRUE)
dim(housing_train)
dim(housing_test)
names(housing_train)
str(housing_train)

# Combine the testing and training datasets, covariates only
housing <- rbind(housing_train[,1:79],housing_test)
dim(housing)

# Removing variables with more than 10% missing data
sort(sapply(housing,function(x) sum(is.na(x))),dec=TRUE)
miss_variables <- names(which(sapply(housing,
                                     function(x) sum(is.na(x)))>0.1*nrow(housing_train)))
miss_variables
length(miss_variables)
housing[,miss_variables] <- NULL
dim(housing)
# Finding the covariate degrees of freedom
find_df <- function(x){
  if(class(x)=="numeric") mdf <- 1
  if(class(x)=="integer") mdf <- 1
  if(class(x) =="factor") mdf <- length(levels(x))
  if(class(x) =="character") mdf <- length(unique(x))
  return(mdf)
}
sapply(housing,find_df)

round(table(housing$Condition2)/nrow(housing),2)
round(table(housing$Exterior1st)/nrow(housing),2)

# Factor Variables Truncation
Truncate_Factor <- function(x,alpha){
  xc <- as.character(x); n <- length(x)
  if(length(unique(x))<=20) {
    critical <- n*alpha
    xc[xc %in% names(which((prop.table(table(xc)))<alpha))] <- "Others"
  }
  xc <- as.factor(xc)
  return(xc)
}
for(i in 1:ncol(housing)){
  if(any(class(housing[,i]) == c('character','factor'))) 
    housing[,i] = Truncate_Factor(housing[,i],0.05)
}
table(housing$Condition2)/nrow(housing)
table(housing$Exterior1st)/nrow(housing)

# Missing Data Imputation
# Imputation for Missing Data in Training Data
housing_impute <- missForest(housing,maxiter = 10,ntree=500,mtry=20)
# The above line runs over several hours on an 8GB RAM Windows 10 machine
save(housing_impute,file='../Data/Housing/housing_covariates_impute.Rdata')
# Create Training Data and Test Data Set from the imputed file
ht_imp <- cbind(housing_impute$ximp[1:nrow(housing_train),],housing_train$SalePrice)
save(ht_imp,file='../Data/Housing/ht_imp.Rdata')
htest_imp <- housing_impute$ximp[(nrow(housing_train)+1):nrow(housing),]
save(htest_imp,file='../Data/Housing/htest_imp.Rdata')


# Visualization and Variable Reduction
load("../Data/Housing/ht_imp_author.Rdata")
names(ht_imp)[69] <- "SalePrice"
SP <- ht_imp$SalePrice
pdf("../Output/Visualizing_Housing_Data.pdf")
for(i in 1:68){
  if(class(ht_imp[,i])=="numeric") {
    plot(ht_imp[,i],SP,xlab=names(ht_imp)[i],ylab="Sales Price")
    title(paste("Scatter plot of Sales Price against ",names(ht_imp)[i]))
  }
  if(class(ht_imp[,i])=="factor") {
    boxplot(SP~ht_imp[,i],xlab=names(ht_imp)[i],ylab="Sales Price",notch=TRUE)
    title(paste("Boxplot of Salesprice by ",names(ht_imp)[i]))
  }
}
dev.off()
table(ht_imp$MSSubClass)
cor(ht_imp[sapply(ht_imp,is.numeric)]) # gives lot of output
cor(ht_imp[sapply(ht_imp,is.numeric)])[,1]


load("../Data/Housing/housing_covariates_impute.Rdata")
housing_covariates <- housing_impute$ximp
housing_cov_famd <- FAMD(housing_covariates,ncp=68,graph=FALSE) 
colnames(housing_cov_famd$eig) <- c("Component","Variance","Cumulative")
housing_cov_famd$eig
windows(height=100,width=200)
pareto.chart(housing_cov_famd$eig[,2])
save(housing_cov_famd,file='../Data/Housing/Housing_FAMD.Rdata')
Housing_FAMD_Data <- housing_cov_famd$ind$coord
save(Housing_FAMD_Data,file='../Data/Housing/Housing_FAMD_Data.Rdata')

# Variable Clustering
Housing_VarClust <- kmeansvar(X.quanti = 
                                housing_covariates[sapply(housing_covariates,
                                                          is.numeric)],
                              X.quali = 
                                housing_covariates[sapply(housing_covariates,
                                                          is.factor)],init=4)
# Renaming levels of housing_covariates
hc2 <- housing_covariates
for(i in 1:ncol(hc2)){
  if(class(hc2[,i])=="factor") {
    hc2[,i] <- mapvalues(hc2[,i],from=levels(hc2[,i]),
                         to=paste0(names(hc2)[i],"_",levels(hc2[,i])))
  }
}
Housing_VarClust <- kmeansvar(X.quanti = hc2[sapply(hc2,is.numeric)],
                              X.quali = hc2[sapply(hc2,is.factor)],
                              init=4)
Housing_VarClust$cluster
summary(Housing_VarClust)
Housing_VarClust$coef


# Base Regression Models
data(galton)
cor(galton)
plot(galton)
head(galton)
cp_lm <- lm(child~parent,data=galton)
summary(cp_lm)

Mantel <- read.csv("../Data/Mantel.csv")
Mantel
Mantel_lm <- lm(Y~.,data=Mantel)
summary(Mantel_lm)

d2 <- cbind(galton,residuals(cp_lm))
names(d2) <- c("child","parent","frankenstein")
cpf_lm <- lm(child~.,d2)
summary(cpf_lm)
d2$frankenstein <- jitter(d2$frankenstein)
summary(lm(child~.,d2))

# Housing Data
load("../Data/Housing/ht_imp_author.Rdata")
load("../Data/Housing/htest_imp_author.Rdata")

Y <- "SalePrice"
X <- names(ht_imp)[-69]
names(ht_imp)[69] <- "SalePrice"
set.seed(12345)
BV <- sample(c("Build","Validate"),nrow(ht_imp),replace = TRUE,
             prob=c(0.7,0.3))
HT_Build <- ht_imp[BV=="Build",]
HT_Validate <- ht_imp[BV=="Validate",]
HT_Formula <- as.formula("SalePrice~.")


## Build linear regression, regression tree, neural network, and LASSO models
# Linear Regression
HT_LM_01 <- lm(HT_Formula,data=HT_Build)
summary(HT_LM_01)
HT_LM_Final <- step(HT_LM_01)
summary(HT_LM_Final)
# Neural Network
HT_NN <- nnet(HT_Formula,data=HT_Build,linout=TRUE,maxit=100,size=5)
summary(HT_NN)
pdf("../Output/Housing_NN.pdf",height = 25, width=60)
plotnet(HT_NN) # very chaotic network
dev.off()
# Regression Tree
HT_rtree <- rpart(HT_Formula,data=HT_Build)
plot(HT_rtree,uniform = TRUE); text(HT_rtree)
HT_rtree$variable.importance
barplot(HT_rtree$variable.importance,las=2,yaxt="n")


# Predicting with validation dataset
HT_LM_01_val_hat <- predict(HT_LM_01,newdata = HT_Validate[,-69])
mean(abs(HT_LM_01_val_hat - HT_Validate$SalePrice)/HT_Validate$SalePrice)
HT_LM_Final_val_hat <- predict(HT_LM_Final,newdata = HT_Validate[,-69])
mean(abs(HT_LM_Final_val_hat - HT_Validate$SalePrice)/HT_Validate$SalePrice)
HT_NN_val_hat <- predict(HT_NN,newdata = HT_Validate[,-69])
mean(abs(HT_NN_val_hat - HT_Validate$SalePrice)/HT_Validate$SalePrice)
HT_rtree_val_hat <- predict(HT_rtree,newdata = HT_Validate[,-69])
mean(abs(HT_rtree_val_hat - HT_Validate$SalePrice)/HT_Validate$SalePrice)

windows(height = 100,width = 100)
plot(HT_Validate$SalePrice,HT_LM_01_val_hat,col="blue",
     xlab="Sales Price",ylab="Predicted Value")
points(HT_Validate$SalePrice,HT_LM_Final_val_hat,col="green")
points(HT_Validate$SalePrice,HT_NN_val_hat,col="red")
points(HT_Validate$SalePrice,HT_rtree_val_hat,col="yellow")
legend(x=6e+05,y=4e+05,lty=3,
       legend=c("Linear","Best Linear","Neural Network","Regression Tree"),
       col=c("blue","green","red","yellow"))


# Bagging the regression models
housing_bagging <- bagging(formula = HT_Formula,data=ht_imp,nbagg=500,
                           coob=TRUE,keepX=TRUE)
housing_bagging$err
pdf("../Output/Housing_Bagging.pdf")
for(i in 1:500){
  temp <- housing_bagging$mtrees[[i]]
  plot(temp$btree)
  text(temp$btree,use.n=TRUE)
}
dev.off()
VI <- data.frame(matrix(0,nrow=500,ncol=ncol(ht_imp)-1))
vnames <- names(ht_imp)[-69]
names(VI) <- vnames
for(i in 1:500){
  VI[i,] <- as.numeric(housing_bagging$mtrees[[i]]$btree$variable.importance[vnames])
}
Bagging_VI <- colMeans(VI,na.rm = TRUE)
Bagging_VI <- sort(Bagging_VI,dec=TRUE)
barplot(Bagging_VI,las=2,yaxt="n")
title("Variable Importance of Bagging")

HT_bagging_val_hat <- predict(housing_bagging,newdata = HT_Validate[,-69])
mean(abs(HT_bagging_val_hat - HT_Validate$SalePrice)/HT_Validate$SalePrice)


# Random forest for the regression tree
housing_RF <- randomForest(formula=HT_Formula,data=ht_imp,ntree=500,
                           replace=TRUE,importance=TRUE)
pdf("../Output/Housing_RF.pdf",height=100,width=500)
plot_RF(housing_RF)
dev.off()
windows(height=100,width=200)
varImpPlot(housing_RF)

HT_RF_val_hat <- predict(housing_RF,newdata = HT_Validate[,-69])
mean(abs(HT_RF_val_hat - HT_Validate$SalePrice)/HT_Validate$SalePrice)


# Gradient Boosting 
housing_gbm <- gbm(formula=HT_Formula,data=HT_Build,distribution = "gaussian",
                   n.trees=1e3,shrinkage = 0.05,keep.data=TRUE,
                   interaction.depth=1,
                   cv.folds=3,n.cores = 1)
summary(housing_gbm)
windows(height=100,width=200)
par(mfrow=c(1,2))
gbm.perf(housing_gbm,method="OOB",plot.it=TRUE,
                             oobag.curve = TRUE,overlay=TRUE)
HT_gbm_val_hat <- predict(housing_gbm,newdata = HT_Validate[,-69])
mean(abs(HT_gbm_val_hat - HT_Validate$SalePrice)/HT_Validate$SalePrice)


# Stacked Ensemble
control <- trainControl(method="repeatedcv", number=10, repeats=3, 
                        savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lm', 'rpart')
set.seed(12345)
Emodels <- caretList(HT_Formula, data=HT_Build, trControl=control, 
                     methodList=algorithmList,
                     tuneList=list(
                       nnet=caretModelSpec(method='nnet', trace=FALSE,
                                           linout=TRUE)
                       
                     )
                     )
Enresults <- resamples(Emodels)
summary(Enresults)
dotplot(Enresults)
modelCor(Enresults)
splom(Enresults)
  
HT_Validate_Predictions <- rowMeans(predict(Emodels,newdata = HT_Validate))
mean(abs(HT_Validate_Predictions - HT_Validate$SalePrice)/HT_Validate$SalePrice)
mean(abs(gt - HT_Validate$SalePrice)/HT_Validate$SalePrice)
