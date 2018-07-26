formula=HT2_Formula
train=HT2_Train
testX=HT2_TestX
testY=HT2_TestY


formula=Waveform_DF_Formula
train=Waveform_DF_Train
testX=Waveform_DF_TestX
testY=Waveform_DF_TestY

Multiple_Model_Fit_SE <- function(formula,train,testX,testY){
  ntr <- nrow(train) # Training size
  nte <- nrow(testX) # Test size
  p <- ncol(testX)
  testY_numeric <- as.numeric(testY)
  
  # Neural Network
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
  HT_Predictions <- data.frame(Actual <- testY_numeric,
                               LR_Pred <- LR_Predict_Bin,
                               NN_Pred <- NN_Predict,
                               NB_Pred <- NB_predict,
                               CT_Pred <- CT_predict,
                               SVM_Pred <- svm_predict
                               )
  names(HT_Predictions) <- c("Actual",paste0(c("LR","NN","NB","CT","SVM"),
                                             "_Pred"))
  HT_Predictions$NN_Pred <- ifelse(HT_Predictions$NN_Pred=="hypothyroid",1,2)
  HT_Predictions$NB_Pred <- ifelse(HT_Predictions$NB_Pred=="hypothyroid",1,2)
  HT_Predictions$CT_Pred <- ifelse(HT_Predictions$CT_Pred=="hypothyroid",1,2)
  HT_Predictions$SVM_Pred <- ifelse(HT_Predictions$SVM_Pred=="hypothyroid",1,2)
  HT_Predictions$SE <- ifelse(rowSums(HT_Predictions[,2:6]==1)>=3,1,2)
  #View(HT_Predictions)
  SE_Accuracy <- sum(HT_Predictions$Actual==HT_Predictions$SE)/nrow(HT_Predictions)
  
  Accu_Mat <- matrix(nrow=6,ncol=2)
  Accu_Mat[,1] <- c("Neural Network","Logistic Regression","Naive Bayes",
                    "Decision Tree","Support Vector Machine","SE")
  Accu_Mat[,2] <- round(c(NN_Accuracy,LR_Accuracy,NB_Accuracy,
                          CT_Accuracy,svm_Accuracy,SE_Accuracy),4)
  return(Accu_Mat)
  
}


Multiple_Model_Fit_SE(formula=HT2_Formula,train=HT2_Train,
                   testX=HT2_TestX,
                   testY=HT2_TestY)

Multiple_Model_Fit_SE(formula=Waveform_DF_Formula,train=Waveform_DF_Train,
                   testX=Waveform_DF_TestX,
                   testY=Waveform_DF_TestY)

Multiple_Model_Fit_SE(formula=GC2_Formula,train=GC2_Train,
                   testX=GC2_TestX,
                   testY =GC2_TestY )

Multiple_Model_Fit_SE(formula=PID_Formula,train=PimaIndiansDiabetes_Train,
                   testX=PimaIndiansDiabetes_TestX,
                   testY=PimaIndiansDiabetes_TestY)

