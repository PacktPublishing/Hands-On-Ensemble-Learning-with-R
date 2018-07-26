library(mlbench)
library(ipred)
library(class)
library(rpart)
install.packages("FNN")
library(FNN)

setwd("D:")
# setwd("/home/pranathi/Documents/ELwR/R_Programs/Chapter_03/SRC")


# German Credit Data 
load("D:\9012\Chapter_03\Data\GC2.RData")
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(GC2),replace = TRUE,prob = c(0.7,0.3))
GC2_Train <- GC2[Train_Test=="Train",]
GC2_TestX <- within(GC2[Train_Test=="Test",],rm(good_bad))
GC2_TestY <- GC2[Train_Test=="Test","good_bad"]
nte <- nrow(GC2_TestX)
GC2_Formula <- as.formula("good_bad~.")

DT_01 <- rpart(GC2_Formula,GC2_Train)
windows(height=200,width=200)
par(mfrow=c(1,2))
plot(DT_01,uniform=TRUE,main="DT - 01"); text(DT_01,use.n=TRUE)
DT_01_predict <- predict(DT_01,newdata = GC2_TestX,type="class")
DT_01_Accuracy <- sum(DT_01_predict==GC2_TestY)/nte
DT_01_Accuracy
summary(DT_01)
DT_01$variable.importance


DT_02 <- rpart(GC2_Formula,GC2_Train,minsplit=30,minbucket=15)
plot(DT_02,uniform=TRUE,main="DT - 02"); text(DT_02,use.n=TRUE)
DT_02_predict <- predict(DT_02,newdata = GC2_TestX,type="class")
DT_02_Accuracy <- sum(DT_02_predict==GC2_TestY)/nte
DT_02_Accuracy
DT_02$variable.importance

DT_03 <- rpart(GC2_Formula,GC2_Train,minsplit=30,minbucket=15,
               cp=0.005)
plot(DT_03,uniform=TRUE,main="DT - 03"); text(DT_03,use.n=TRUE)
DT_03_predict <- predict(DT_03,newdata = GC2_TestX,type="class")
DT_03_Accuracy <- sum(DT_03_predict==GC2_TestY)/nte
DT_03_Accuracy
DT_03$variable.importance

DT_04 <- rpart(GC2_Formula,GC2_Train,minsplit=30,minbucket=15,
               parms = list(split="information",
                            loss=matrix(c(0,200,500,0),byrow = TRUE,nrow=2)))
plot(DT_04,uniform=TRUE,main="DT - 04"); text(DT_04,use.n=TRUE)
DT_04_predict <- predict(DT_04,newdata = GC2_TestX,type="class")
DT_04_Accuracy <- sum(DT_04_predict==GC2_TestY)/nte
DT_04_Accuracy
DT_04$variable.importance

# Bagging
# Sample Probability
N <- 11:100
B <- 1e5
Prob_Avg <- NULL
for(i in N){
  set <- 1:i
  leftout <- 0
  for(j in 1:B){
    s1 <- sample(set,i,replace=TRUE)
    leftout <- leftout+(i-length(unique(s1)))/i
  }
  Prob_Avg[i-10] <- leftout/B
}
Prob_Avg

B <- 500
GC2_Bagging <- bagging(GC2_Formula,data=GC2_Train,coob=FALSE,
                       nbagg=B,keepX=TRUE)
GC2_Margin <- predict(GC2_Bagging,newdata = GC2_TestX,
                      aggregation="weighted",type="class")
sum(GC2_Margin==GC2_TestY)/nte
pdf("../Output/GC2_Bagging_Trees.pdf")
for(i in 1:B){
  tt <- GC2_Bagging$mtrees[[i]]
  plot(tt$btree)
  text(tt$btree,use.n=TRUE)
}
dev.off()

VI <- data.frame(matrix(0,nrow=B,ncol=ncol(GC2)-1))
vnames <- names(GC2)[-20]
names(VI) <- vnames
for(i in 1:B){
  VI[i,] <- GC2_Bagging$mtrees[[i]]$btree$variable.importance[vnames]
}
colMeans(VI)
sapply(VI,function(x) sum(is.na(x)))
colMeans(VI,na.rm=TRUE)

GC2_Bagging_02 <- bagging(GC2_Formula,data=GC2_Train,coob=FALSE,
                       nbagg=B,keepX=TRUE,
                       control=rpart.control(minsplit=30,minbucket=15,
                                             split="information",
                                             loss=matrix(c(0,200,500,0),
                                                         byrow = TRUE,
                                                         nrow=2)))
GC2_Margin_02 <- predict(GC2_Bagging_02,newdata = GC2_TestX,
                      aggregation="weighted",type="class")
sum(GC2_Margin_02==GC2_TestY)/nte
pdf("../Output/GC2_Bagging_Trees_02.pdf")
for(i in 1:B){
  tt <- GC2_Bagging_02$mtrees[[i]]
  plot(tt$btree)
  text(tt$btree,use.n=TRUE)
}
dev.off()

VI_02 <- data.frame(matrix(0,nrow=B,ncol=ncol(GC2)-1))
names(VI_02) <- vnames
for(i in 1:B){
  VI_02[i,] <- GC2_Bagging_02$mtrees[[i]]$btree$variable.importance[vnames]
}
colMeans(VI_02,na.rm=TRUE)


# More bags, more accuracy? 
Bags <- c(1:24,seq(25,B,25))
Bag_Acc <- NULL
for(i in 1:length(Bags)){
  TBAG <- bagging(GC2_Formula,data=GC2_Train,coob=FALSE,
                      nbagg=i,keepX=TRUE,
                      control=rpart.control(minsplit=30,minbucket=15,
                                            split="information",
                                            loss=matrix(c(0,200,500,0),
                                                        byrow = TRUE,
                                                        nrow=2)))
  GC2_Margin_TBAG <- predict(TBAG,newdata = GC2_TestX,
                           aggregation="weighted",type="class")
  Bag_Acc[i] <- sum(GC2_Margin_TBAG==GC2_TestY)/nte
  print(Bags[i])
}
plot(Bags,Bag_Acc,"l",ylab="Accuracy")


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

PID_Bagging <- bagging(PID_Formula,data=PimaIndiansDiabetes_Train,coob=FALSE,
                       nbagg=1000,keepX=TRUE)
for(i in 1:200) {
  plot(PID_Bagging$mtrees[[i]]$btree)
  text(PID_Bagging$mtrees[[i]]$btree,pretty=1,use.n=T)
}
PID_Margin <- predict(PID_Bagging,newdata = PimaIndiansDiabetes_TestX,
                      aggregation="weighted",type="class")
sum(PID_Margin==PimaIndiansDiabetes_TestY)/257


# k-NN Classifier
set.seed(123)
Waveform <- mlbench.waveform(5000)
Waveform$classes <- ifelse(Waveform$classes!=3,1,2)
Waveform_DF <- data.frame(cbind(Waveform$x,Waveform$classes)) # Data Frame
names(Waveform_DF) <- c(paste0("X",".",1:21),"Classes")
Waveform_DF$Classes <- as.factor(Waveform_DF$Classes)
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(Waveform_DF),replace = TRUE,prob = c(0.7,0.3))
Waveform_DF_Train <- Waveform_DF[Train_Test=="Train",]
Waveform_DF_TestX <- within(Waveform_DF[Train_Test=="Test",],rm(Classes))
Waveform_DF_TestY <- Waveform_DF[Train_Test=="Test","Classes"]
Waveform_DF_Formula <- as.formula("Classes~.")

plot(Waveform_DF_Train$X.1,Waveform_DF_Train$X.8,col=Waveform_DF_Train$Classes)
WF_knn <- knn(train=Waveform_DF_Train[,-22],test=Waveform_DF_TestX,
              cl=Waveform_DF_Train$Classes,k=10)
sum(Waveform_DF_TestY==WF_knn)/nrow(Waveform_DF_TestX)
k <- c(2:15,seq(20,50,5))
knn_accuracy <- NULL
for(i in 1:length(k)){
  WF_temp_knn <- knn(train=Waveform_DF_Train[,-22],test=Waveform_DF_TestX,
                cl=Waveform_DF_Train$Classes,k=k[i])
  knn_accuracy <- c(knn_accuracy,sum(Waveform_DF_TestY==WF_temp_knn)/
                      nrow(Waveform_DF_TestX))
}
knn_accuracy

# k-NN Bagging
All_Cov <- rbind(GC2_Train[,-20],GC2_TestX)
All_CovX <- model.matrix(~.-1,All_Cov)
GC2_Train_Cov <- All_CovX[1:nrow(GC2_Train),]
GC2_Test_Cov <- All_CovX[(nrow(GC2_Train)+1):nrow(All_CovX),]
k <- seq(5,50,1)
knn_accuracy <- NULL
for(i in 1:length(k)){
  GC2_knn_Bagging <- ownn(train=GC2_Train_Cov, test=GC2_Test_Cov,
                          cl=GC2_Train$good_bad,testcl=GC2_TestY,k=k[i])
 knn_accuracy[i] <- GC2_knn_Bagging$accuracy[3]
}
knn_accuracy
windows(height=100,width=100)
plot.ts(knn_accuracy,main="k-NN Accuracy")

#GC2_Test_Cov <- model.matrix(~.-1,GC2_TestX)


