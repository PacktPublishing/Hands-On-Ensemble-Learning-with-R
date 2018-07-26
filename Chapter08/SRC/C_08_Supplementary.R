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




