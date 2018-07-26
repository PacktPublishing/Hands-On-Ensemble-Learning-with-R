library(ACSWR)
library(caret)
library(e1071)
library(factoextra)
library(mlbench)
library(NeuralNetTools)
library(nnet)
library(pROC)
library(RSADBE)
library(survival)
library(rpart)
setwd("C:/Users/tprabhan/Documents/My_Books/ELwR/R_Programs/Chapter_01/SRC")

# Hypothroid Dataset
HT <- read.csv("../Data/Hypothyroid.csv",header = TRUE,stringsAsFactors = F)
HT$Hypothyroid <- as.factor(HT$Hypothyroid)
HT2 <- HT[,c("Hypothyroid","Age","Gender","TSH","T3","TT4","T4U","FTI")]
sapply(HT2,function(x) sum(is.na(x)))
HT2 <- na.omit(HT2)
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(HT2),replace = TRUE,prob = c(0.7,0.3))
head(Train_Test)
HT2_Train <- HT2[Train_Test=="Train",]
HT2_TestX <- within(HT2[Train_Test=="Test",],rm(Hypothyroid))
HT2_TestY <- HT2[Train_Test=="Test",c("Hypothyroid")]
HT2_Formula <- as.formula("Hypothyroid~.")

# Waveform Dataset
set.seed(123)
Waveform <- mlbench.waveform(5000)
table(Waveform$classes)
Waveform$classes <- ifelse(Waveform$classes!=3,1,2)
Waveform_DF <- data.frame(cbind(Waveform$x,Waveform$classes)) # Data Frame
names(Waveform_DF) <- c(paste0("X",".",1:21),"Classes")
Waveform_DF$Classes <- as.factor(Waveform_DF$Classes)
table(Waveform_DF$Classes)
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(Waveform_DF),replace = TRUE,prob = c(0.7,0.3))
head(Train_Test)
Waveform_DF_Train <- Waveform_DF[Train_Test=="Train",]
Waveform_DF_TestX <- within(Waveform_DF[Train_Test=="Test",],rm(Classes))
Waveform_DF_TestY <- Waveform_DF[Train_Test=="Test","Classes"]
Waveform_DF_Formula <- as.formula("Classes~.")


# German Credit Dataset
load("../Data/GC2.RData")
table(GC2$good_bad)
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(GC2),replace = TRUE,prob = c(0.7,0.3))
head(Train_Test)
GC2_Train <- GC2[Train_Test=="Train",]
GC2_TestX <- within(GC2[Train_Test=="Test",],rm(good_bad))
GC2_TestY <- GC2[Train_Test=="Test","good_bad"]
GC2_Formula <- as.formula("good_bad~.")

# IRIS Dataset
data("iris")
ir2 <- iris
ir2$Species <- ifelse(ir2$Species=="setosa","S","NS")
ir2$Species <- as.factor(ir2$Species)
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(ir2),replace = TRUE,prob = c(0.7,0.3))
head(Train_Test)
ir2_Train <- ir2[Train_Test=="Train",]
ir2_TestX <- within(ir2[Train_Test=="Test",],rm(Species))
ir2_TestY <- ir2[Train_Test=="Test","Species"]
ir2_Formula <- as.formula("Species~.")

# Diabetes Dataset
data("PimaIndiansDiabetes")
#PimaIndiansDiabetes$diabetes <- as.character(PimaIndiansDiabetes$diabetes)
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(PimaIndiansDiabetes),replace = TRUE,prob = c(0.7,0.3))
head(Train_Test)
PimaIndiansDiabetes_Train <- PimaIndiansDiabetes[Train_Test=="Train",]
PimaIndiansDiabetes_TestX <- within(PimaIndiansDiabetes[Train_Test=="Test",],
          rm(diabetes))
PimaIndiansDiabetes_TestY <- PimaIndiansDiabetes[Train_Test=="Test","diabetes"]
PID_Formula <- as.formula("diabetes~.")

# US Crime
data(usc)
str(usc)
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(usc),replace = TRUE,prob = c(0.7,0.3))
head(Train_Test)
usc_Train <- usc[Train_Test=="Train",]
usc_TestX <- within(usc[Train_Test=="Test",],rm(R))
usc_TestY <- usc[Train_Test=="Test","R"]
usc_Formula <- as.formula("R~.")

# New Zealand OVerseas Visitors
# osvisit <- read.csv("../Data/osvisit.dat", header= FALSE)
# osv <- ts(osvisit$V1, start = 1977, frequency = 12)
# class(osv)
# plot.ts(osv)
# jpeg("../Output/Overseas.jpeg")
# dev.off()
# Primary Biliary Cirrhosis
data(pbc)
names(pbc)

# Multishapes
data("multishapes")
table(multishapes$shape)
plot(multishapes[,1],multishapes[,2],col=multishapes[,3])

# Board Stiffness
data(stiff)
sort(mahalanobis(stiff,colMeans(stiff),cov(stiff)),decreasing = TRUE)


#######################################
#  Statistical/Machine Learning Models #
########################################
ntr <- nrow(HT2_Train) # Training size
nte <- nrow(HT2_TestX) # Test size
p <- ncol(HT2_TestX)
testY_numeric <- as.numeric(HT2_TestY)
LR_fit <- glm(HT2_Formula,data=HT2_Train,family = binomial())
summary(LR_fit)
LR_Predict <- predict(LR_fit,newdata=HT2_TestX,type="response")
LR_Predict_Bin <- ifelse(LR_Predict>0.5,2,1)
LR_Accuracy <- sum(LR_Predict_Bin==testY_numeric)/nte
LR_Accuracy

plotnet(rep(0,17),struct=c(3,2,2,1))
title("A Neural Network with Two Hidden Layers")
set.seed(12345)
NN_fit <- nnet(HT2_Formula,data = HT2_Train,size=p,trace=FALSE)
NN_Predict <- predict(NN_fit,newdata=HT2_TestX,type="class")
NN_Accuracy <- sum(NN_Predict==HT2_TestY)/nte
NN_Accuracy
plotnet(NN_fit)
title("Neural Network for Hypothyroid Classification")

NB_fit <- naiveBayes(HT2_Formula,data=HT2_Train)
NB_predict <- predict(NB_fit,newdata=HT2_TestX)
NB_Accuracy <- sum(NB_predict==HT2_TestY)/nte
NB_Accuracy

CT_fit <- rpart(HT2_Formula,data=HT2_Train)
plot(CT_fit,uniform=TRUE)
text(CT_fit)
CT_predict <- predict(CT_fit,newdata=HT2_TestX,type="class")
CT_Accuracy <- sum(CT_predict==HT2_TestY)/nte
CT_Accuracy

SVM_fit <- svm(HT2_Formula,data=HT2_Train)
SVM_predict <- predict(SVM_fit,newdata=HT2_TestX,type="class")
SVM_Accuracy <- sum(SVM_predict==HT2_TestY)/nte
SVM_Accuracy


# The Right Model Dilemma!
Multiple_Model_Fit <- function(formula,train,testX,testY){
  ntr <- nrow(train) # Training size
  nte <- nrow(testX) # Test size
  p <- ncol(testX)
  testY_numeric <- as.numeric(testY)
  
  # Neural Network
  set.seed(12345)
  NN_fit <- nnet(formula,data = train,size=p,trace=FALSE)
  NN_Predict <- predict(NN_fit,newdata=testX,type="class")
  NN_Accuracy <- sum(NN_Predict==testY)/nte
  
  # Logistic Regressiona
  LR_fit <- glm(formula,data=train,family = binomial())
  LR_Predict <- predict(LR_fit,newdata=testX,type="response")
  LR_Predict_Bin <- ifelse(LR_Predict>0.5,2,1)
  LR_Accuracy <- sum(LR_Predict_Bin==testY_numeric)/nte
  
  # Naive Bayes
  NB_fit <- naiveBayes(formula,data=train)
  NB_predict <- predict(NB_fit,newdata=testX)
  NB_Accuracy <- sum(NB_predict==testY)/nte
  
  # Decision Tree
  CT_fit <- rpart(formula,data=train)
  CT_predict <- predict(CT_fit,newdata=testX,type="class")
  CT_Accuracy <- sum(CT_predict==testY)/nte
  
  # Support Vector Machine
  svm_fit <- svm(formula,data=train)
  svm_predict <- predict(svm_fit,newdata=testX,type="class")
  svm_Accuracy <- sum(svm_predict==testY)/nte
  
  Accu_Mat <- matrix(nrow=5,ncol=2)
   Accu_Mat[,1] <- c("Neural Network","Logistic Regression","Naive Bayes",
   "Decision Tree","Support Vector Machine")
  Accu_Mat[,2] <- round(c(NN_Accuracy,LR_Accuracy,NB_Accuracy,
       CT_Accuracy,svm_Accuracy),4)
  return(Accu_Mat)
  
}

Multiple_Model_Fit(formula=HT2_Formula,train=HT2_Train,
      testX=HT2_TestX,
      testY=HT2_TestY)

Multiple_Model_Fit(formula=Waveform_DF_Formula,train=Waveform_DF_Train,
      testX=Waveform_DF_TestX,
      testY=Waveform_DF_TestY)

Multiple_Model_Fit(formula=GC2_Formula,train=GC2_Train,
      testX=GC2_TestX,
      testY =GC2_TestY )

Multiple_Model_Fit(formula=ir2_Formula,train=ir2_Train,
      testX=ir2_TestX,
      testY=ir2_TestY)

Multiple_Model_Fit(formula=PID_Formula,train=PimaIndiansDiabetes_Train,
      testX=PimaIndiansDiabetes_TestX,
      testY=PimaIndiansDiabetes_TestY)


# An Ensemble Purview
load("../Data/GC2.RData")
set.seed(12345)
Train_Test_Stack <- sample(c("Train","Test","Stack"),nrow(GC2),replace = TRUE,prob = c(0.5,0.25,0.25))
GC2_Train <- GC2[Train_Test_Stack=="Train",]
GC2_Test <- GC2[Train_Test_Stack=="Test",]
GC2_Stack <- GC2[Train_Test_Stack=="Stack",]

# set label name and Exhogenous
Endogenous <- 'good_bad'
Exhogenous <- names(GC2_Train)[names(GC2_Train) != Endogenous]

# create a caret control object to control the number of cross-validations performed
myControl <- trainControl(method='cv', number=3, returnResamp='none')

# train all the ensemble models with GC2_Train
model_NB <- train(GC2_Train[,Exhogenous], GC2_Train[,Endogenous], 
                   method='naive_bayes', trControl=myControl)
model_rpart <- train(GC2_Train[,Exhogenous], GC2_Train[,Endogenous], 
                     method='rpart', trControl=myControl)
model_glm <- train(GC2_Train[,Exhogenous], GC2_Train[,Endogenous], 
                       method='glm', trControl=myControl)

# get predictions for each ensemble model for two last data sets
# and add them back to themselves
GC2_Test$NB_PROB <- predict(object=model_NB, GC2_Test[,Exhogenous],
                             type="prob")[,1]
GC2_Test$rf_PROB <- predict(object=model_rpart, GC2_Test[,Exhogenous],
                            type="prob")[,1]
GC2_Test$glm_PROB <- predict(object=model_glm, 
                                 GC2_Test[,Exhogenous],
                                 type="prob")[,1]
GC2_Stack$NB_PROB <- predict(object=model_NB, GC2_Stack[,Exhogenous],
                              type="prob")[,1]
GC2_Stack$rf_PROB <- predict(object=model_rpart, GC2_Stack[,Exhogenous],
                             type="prob")[,1]
GC2_Stack$glm_PROB <- predict(object=model_glm, 
                                  GC2_Stack[,Exhogenous],
                                  type="prob")[,1]

# see how each individual model performed on its own
AUC_NB <- roc(GC2_Test[,Endogenous], GC2_Test$NB_PROB )
AUC_NB$auc
AUC_rf <- roc(GC2_Test[,Endogenous], GC2_Test$rf_PROB )
AUC_rf$auc
AUC_glm <- roc(GC2_Test[,Endogenous], GC2_Test$glm_PROB )
AUC_glm$auc

# Stacking it together
Exhogenous2 <- names(GC2_Stack)[names(GC2_Stack) != Endogenous]
Stack_Model <- train(GC2_Stack[,Exhogenous2], GC2_Stack[,Endogenous], 
                     method='naive_bayes', trControl=myControl)
Stack_Prediction <- predict(object=Stack_Model,GC2_Test[,Exhogenous2],type="prob")[,1]
Stack_AUC <- roc(GC2_Test[,Endogenous],Stack_Prediction)
Stack_AUC$auc


# Complementary Statistical Tests
# Permutation Test
library(perm)
x <- c(18,20,22); y <- c(24,26,28)
t.test(x,y,var.equal = TRUE)
permTS(x,y)
x2 <- c(16,18,20,22); y2 <- c(24,26,28,30)
t.test(x2,y2,var.equal = TRUE)
permTS(x2,y2)

# Chi-square and McNemar's Test
table(LR_Predict_Bin,testY_numeric)
table(NN_Predict,HT2_TestY)
table(NB_predict,HT2_TestY)
table(CT_predict,HT2_TestY)
table(SVM_predict,HT2_TestY)
chisq.test(table(LR_Predict_Bin,testY_numeric))
chisq.test(table(NN_Predict,HT2_TestY))
chisq.test(table(NB_predict,HT2_TestY))
chisq.test(table(CT_predict,HT2_TestY))
chisq.test(table(SVM_predict,HT2_TestY))
mcnemar.test(table(LR_Predict_Bin,testY_numeric))
mcnemar.test(table(NN_Predict,HT2_TestY))
mcnemar.test(table(NB_predict,HT2_TestY))
mcnemar.test(table(CT_predict,HT2_TestY))
mcnemar.test(table(SVM_predict,HT2_TestY))

# ROC Test
HT_NN_Prob <- predict(NN_fit,newdata=HT2_TestX,type="raw")
HT_NN_roc <- roc(HT2_TestY,c(HT_NN_Prob))
HT_NN_roc$auc
HT_CT_Prob <- predict(CT_fit,newdata=HT2_TestX,type="prob")[,2]
HT_CT_roc <- roc(HT2_TestY,HT_CT_Prob)
HT_CT_roc$auc
roc.test(HT_NN_roc,HT_CT_roc)


