library(gbm)
library(xgboost)
library(ada)
library(adabag)
library(h2o)
library(kernlab)
library(DiagrammeR)
library(Matrix)
setwd("C:/Users/tprabhan/Documents/My_Books/ELwR/R_Programs/Chapter_06/SRC")


data("spam")
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(spam),replace = TRUE,
                     prob = c(0.7,0.3))
spam_Train <- spam[Train_Test=="Train",]
spam_Formula <- as.formula("type~.")
spam_b0 <- boosting(spam_Formula,data=spam_Train,mfinal=1)
sum(predict(spam_b0,newdata=spam_Train)$class==
      spam_Train$type)/nrow(spam_Train)
mb0 <- margins(spam_b0,newdata=spam_Train)$margins
mb0[1:20]
summary(mb0)
spam_b1 <- boosting(spam_Formula,data=spam_Train,mfinal=5)
sum(predict(spam_b1,newdata=spam_Train)$class==
      spam_Train$type)/nrow(spam_Train)
mb1 <- margins(spam_b1,newdata=spam_Train)$margins
mb1[1:20]
summary(mb1)
spam_b2 <- boosting(spam_Formula,data=spam_Train,mfinal=10)
sum(predict(spam_b2,newdata=spam_Train)$class==
      spam_Train$type)/nrow(spam_Train)
mb2 <- margins(spam_b2,newdata=spam_Train)$margins
mb2[1:20]
summary(mb2)
spam_b3 <- boosting(spam_Formula,data=spam_Train,mfinal=20)
sum(predict(spam_b3,newdata=spam_Train)$class==
      spam_Train$type)/nrow(spam_Train)
mb3 <- margins(spam_b3,newdata=spam_Train)$margins
mb3[1:20]
summary(mb3)
spam_b4<- boosting(spam_Formula,data=spam_Train,mfinal=50)
sum(predict(spam_b4,newdata=spam_Train)$class==
      spam_Train$type)/nrow(spam_Train)
mb4 <- margins(spam_b4,newdata=spam_Train)$margins
mb4[1:20]
summary(mb4)
spam_b5<- boosting(spam_Formula,data=spam_Train,mfinal=200)
sum(predict(spam_b5,newdata=spam_Train)$class==
      spam_Train$type)/nrow(spam_Train)
mb5 <- margins(spam_b5,newdata=spam_Train)$margins
mb5[1:20]
summary(mb5)
View(cbind(mb1,mb2,mb3,mb4,mb5)[mb1<0,])



# The gbm Package
spam_Train2 <- spam_Train
spam_Train2$type <- as.numeric(spam_Train2$type)-1
spam_gbm <- gbm(spam_Formula,distribution="bernoulli",data=spam_Train2,
                  n.trees=500,bag.fraction = 0.8,shrinkage = 0.1)
plot(spam_gbm) # output suppressed
summary(spam_gbm)
spam_gbm
spam_gbm2 <- gbm(spam_Formula,distribution="bernoulli",data=spam_Train2,
                n.trees=500,bag.fraction = 0.8,shrinkage = 0.0001)
spam_gbm2
windows(height=100,width=200)
par(mfrow=c(1,2))
gbm.perf(spam_gbm,plot.it=TRUE)
gbm.perf(spam_gbm2,plot.it=TRUE)


#### Loss Functions and Examples
# Poisson regression and boosting
# https://stats.idre.ucla.edu/r/dae/poisson-regression/
pregnancy <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
pregnancy <- within(pregnancy, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(pregnancy)
pregnancy_Poisson <- glm(num_awards ~ prog + math, 
                    family="poisson", data=pregnancy)
summary(pregnancy_Poisson)
pregnancy_boost <- gbm(num_awards ~ prog+math,dist="poisson",n.trees=100,
              interaction.depth = 2,shrinkage=0.1,data=pregnancy)

cbind(pregnancy$num_awards,predict(m1,type="response"),
      predict(pboost,n.trees=100,type="response"))
sum((pregnancy$num_awards-predict(m1,type="response"))^2)
sum((pregnancy$num_awards-predict(pboost,n.trees=100,type="response"))^2)
summary(pregnancy_boost)
pretty.gbm.tree(pregnancy_boost,i.tree=18)
pretty.gbm.tree(pregnancy_boost,i.tree=63)


# Survival data
pbc_boost <- gbm(Surv(time,status==2)~trt + age + sex + ascites + 
                   hepato + spiders + edema + bili + chol + albumin + 
                   copper + alk.phos + ast + trig + platelet + 
                   protime + stage,n.trees=100,
                 interaction.depth = 2,shrinkage=0.01,
                 dist="coxph",data=pbc)
summary(pbc_boost)
pretty.gbm.tree(pbc_boost,i.tree=2)
pretty.gbm.tree(pbc_boost,i.tree=72)


## The xgboost Package
data("spam")
spam2 <- spam
spam2$type <- as.numeric(spam2$type)-1
head(data.frame(spam2$type,spam$type))
# 1 denotes spam, and 0 - nonspam
set.seed(12345)
Train_Test <- sample(c("Train","Test"),nrow(spam2),replace = TRUE,
                     prob = c(0.7,0.3))
spam2_Train <- spam2[Train_Test=="Train",]
spam_Formula <- as.formula("type~.")
spam_train <- list()
spam_train$data <- as.matrix(spam2_Train[,-58])
colnames(spam_train$data) <- names(spam)[-58]
spam_train$data <- as(spam_train$data,"dgCMatrix")
spam_train$label <- spam2_Train$type
class(spam_train$data)
# Simple XGBoosting
spam_xgb <- xgboost(data=spam_train$data,label=spam_train$label,
                    nrounds = 100,objective="binary:logistic")
xgb_predict <- predict(spam_xgb,spam_train$data)
sum(xgb_predict>0.5)
sum(spam_train$label)
table(spam_train$label,c(xgb_predict>0.5))
# XGBoosting with cross-validation
spam_xgb_cv <- xgb.cv(data=spam_train$data,label=spam_train$label,
                   nfold=10,nrounds = 100,objective="binary:logistic",
                   prediction = TRUE)
xgb_cv_predict <- spam_xgb_cv$pred
sum(xgb_cv_predict>0.5)
table(spam_train$label,c(xgb_cv_predict>0.5))

# Stop early
spam_xgb_cv2 <- xgb.cv(data=spam_train$data,label=spam_train$label,
                       early_stopping_rounds = 5,
                      nfold=10,nrounds = 100,objective="binary:logistic",
                      prediction = TRUE)

# Continue training
xgboost(xgb_model=spam_xgb,
        data=spam_train$data,label=spam_train$label,
        nrounds = 10)


# Variable Importance
xgb.plot.importance(xgb.importance(names(spam_train$data),
                                   spam_xgb)[1:10,])


# Visualizing Trees of xgboost
txgb <- xgboost(data=spam_train$data,label=spam_train$label,
                max.depth = 2,eta = 1, nthread = 2, nround = 2,
                    objective="binary:logistic")
xgb.plot.tree(feature_names = spam_train$data@Dimnames[[2]], 
              model = txgb)




