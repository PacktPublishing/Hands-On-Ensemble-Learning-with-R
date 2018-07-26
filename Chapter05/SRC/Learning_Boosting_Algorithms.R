# LEARNING GRADIENT BOOSTING ALGORITHM

library(rpart)
library(gbm)


setwd("C:/Users/tprabhan/Documents/My_Books/ELwR/R_Programs/Chapter_05/SRC")


# ADAPTIVE BOOSTING with a Toy Dataset
# https://www.youtube.com/watch?v=gmok1h8wG-Q 
# Jessica Noss
# The Toy Data
x1 <- c(1,5,3,1,5)
x2 <- c(5,5,3,1,1)
y <- c(1,1,-1,1,1)
names(y) <- paste0("P",1:5)
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="The TOY Data Depiction")
text(x1,x2,labels=names(y),pos=1)
# Visualizing the stump models
windows(height=200,width=300)
par(mfrow=c(2,3))
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X1<2")
text(x1,x2,labels=names(y),pos=1)
plim <- par("usr")
rect(xleft=2,ybottom = plim[3],xright = plim[2],ytop = plim[4],
     border = "red",col="red",density=20 )
rect(xleft=plim[1],ybottom = plim[3],xright = 2,ytop = plim[4],
     border = "green",col="green",density=20 )
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X1<4")
text(x1,x2,labels=names(y),pos=1)
rect(xleft=4,ybottom = plim[3],xright = plim[2],ytop = plim[4],
     border = "red",col="red",density=20 )
rect(xleft=plim[1],ybottom = plim[3],xright = 4,ytop = plim[4],
     border = "green",col="green",density=20 )
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X1<6")
text(x1,x2,labels=names(y),pos=1)
rect(xleft=6,ybottom = plim[3],xright = plim[2],ytop = plim[4],
     border = "red",col="red",density=20 )
rect(xleft=plim[1],ybottom = plim[3],xright = 6,ytop = plim[4],
     border = "green",col="green",density=20 )
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X1>2")
text(x1,x2,labels=names(y),pos=1)
rect(xleft=2,ybottom = plim[3],xright = plim[2],ytop = plim[4],
     border = "green",col="green",density=20 )
rect(xleft=plim[1],ybottom = plim[3],xright = 2,ytop = plim[4],
     border = "red",col="red",density=20 )
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X1>4")
text(x1,x2,labels=names(y),pos=1)
rect(xleft=4,ybottom = plim[3],xright = plim[2],ytop = plim[4],
     border = "green",col="green",density=20 )
rect(xleft=plim[1],ybottom = plim[3],xright = 4,ytop = plim[4],
     border = "red",col="red",density=20 )
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X1>6")
text(x1,x2,labels=names(y),pos=1)
rect(xleft=6,ybottom = plim[3],xright = plim[2],ytop = plim[4],
     border = "green",col="green",density=20 )
rect(xleft=plim[1],ybottom = plim[3],xright = 6,ytop = plim[4],
     border = "red",col="red",density=20 )

# Classification with x2
windows(height=200,width=300)
par(mfrow=c(2,3))
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X2<2")
text(x1,x2,labels=names(y),pos=1)
plim <- par("usr")
rect(xleft=plim[1],ybottom = plim[3],xright = plim[2],ytop = 2,
     border = "green",col="green",density=20 )
rect(xleft=plim[1],ybottom = 2,xright = plim[2],ytop = plim[4],
     border = "red",col="red",density=20 )
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X2<4")
text(x1,x2,labels=names(y),pos=1)
rect(xleft=plim[1],ybottom = plim[3],xright = plim[2],ytop =4,
     border = "green",col="green",density=20 )
rect(xleft=plim[1],ybottom = 4,xright = plim[2],ytop = plim[4],
     border = "red",col="red",density=20 )
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X2<6")
text(x1,x2,labels=names(y),pos=1)
rect(xleft=plim[1],ybottom = plim[3],xright = plim[2],ytop = 6,
     border = "green",col="green",density=20 )
rect(xleft=plim[1],ybottom = 6,xright = plim[2],ytop = plim[4],
     border = "red",col="red",density=20 )
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X2>2")
text(x1,x2,labels=names(y),pos=1)
plim <- par("usr")
rect(xleft=plim[1],ybottom = plim[3],xright = plim[2],ytop = 2,
     border = "red",col="red",density=20 )
rect(xleft=plim[1],ybottom = 2,xright = plim[2],ytop = plim[4],
     border = "green",col="green",density=20 )
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X2>4")
text(x1,x2,labels=names(y),pos=1)
rect(xleft=plim[1],ybottom = plim[3],xright = plim[2],ytop =4,
     border = "red",col="red",density=20 )
rect(xleft=plim[1],ybottom = 4,xright = plim[2],ytop = plim[4],
     border = "green",col="green",density=20 )
plot(x1,x2,pch=c("+","+","-","+","+"),cex=2,
     xlim=c(0,6),ylim=c(0,6),
     xlab=expression(x[1]),ylab=expression(x[2]),
     main="Classification with Stump X2>6")
text(x1,x2,labels=names(y),pos=1)
rect(xleft=plim[1],ybottom = plim[3],xright = plim[2],ytop = 6,
     border = "red",col="red",density=20 )
rect(xleft=plim[1],ybottom = 6,xright = plim[2],ytop = plim[4],
     border = "green",col="green",density=20 )

# The Simple Stump Models PREDICTIONS
M1 <- c(1,-1,-1,1,-1)   # M1 = X1<2 predicts 1, else -1
M2 <- c(1,-1,1,1,-1)    # M2 = X1<4 predicts 1, else -1
M3 <- c(1,1,1,1,1)      # M3 = X1<6 predicts 1, else -1
M4 <- c(-1,1,1,-1,1)    # M4 = X1>2 predicts 1, else -1;M4=-1*M1
M5 <- c(-1,1,-1,-1,1)   # M5 = X1>4 predicts 1, else -1;M5=-1*M2
M6 <- c(-1,-1,-1,-1,-1) # M6 = X1>6 predicts 1, else -1;M6=-1*M3

# Stem Model Errors
Err_M1 <- M1!=y
Err_M2 <- M2!=y
Err_M3 <- M3!=y
Err_M4 <- M4!=y
Err_M5 <- M5!=y
Err_M6 <- M6!=y
# Their Misclassifications
rbind(Err_M1,Err_M2,Err_M3,Err_M4,Err_M5,Err_M6)

# ROUND 1
# Weighted Error Computation
weights_R1 <- rep(1/length(y),length(y)) #Initializaing the weights
Err_R1 <- rbind(Err_M1,Err_M2,Err_M3,Err_M4,Err_M5,Err_M6)%*%
  weights_R1
Err_R1 # Error rates
# The best classifier error rate
err_rate_r1 <- min(Err_R1)
alpha_3 <- 0.5*log((1-err_rate_r1)/err_rate_r1)
alpha_3
alpha_3*M3
sign(alpha_3*M3)

# Weights Update Formula and Function
Weights_update <- function(weights,error,error_rate){
  weights_new <- NULL
  for(i in 1:length(weights)){
    if(error[i]==FALSE) weights_new[i] <- 0.5*weights[i]/(1-error_rate)
    if(error[i]==TRUE) weights_new[i] <- 0.5*weights[i]/error_rate
  }
  return(weights_new)
}

# ROUND 2
# Update the weights and redo the analyses
weights_R2 <- Weights_update(weights=weights_R1,error=Err_M3,
                             error_rate=err_rate_r1)
Err_R2 <- rbind(Err_M1,Err_M2,Err_M3,Err_M4,Err_M5,Err_M6)%*%
  weights_R2
Err_R2 # Error rates
err_rate_r2 <- min(Err_R2)
alpha_1 <- 0.5*log((1-err_rate_r2)/err_rate_r2)
alpha_1
alpha_3*M3+alpha_1*M1
sign(alpha_3*M3+alpha_1*M1)

# ROUND 3
# Update the weights and redo the analyses
weights_R3 <- Weights_update(weights=weights_R2,error=Err_M1,
                             error_rate=err_rate_r2)
Err_R3 <- rbind(Err_M1,Err_M2,Err_M3,Err_M4,Err_M5,Err_M6)%*%
  weights_R3
Err_R3 # Error rates
err_rate_r3 <- min(Err_R3)
alpha_5 <- 0.5*log((1-err_rate_r3)/err_rate_r3)
alpha_5
alpha_3*M3+alpha_1*M1+alpha_5*M5
sign(alpha_3*M3+alpha_1*M1+alpha_5*M5)



# GRADIENT BOOSTING ALGORITHM
getNode <- function(x,y)	{
  xu <- sort(unique(x),decreasing=TRUE)
  ss <- numeric(length(xu)-1)
  for(i in 1:length(ss))	{
    partR <- y[x>xu[i]]
    partL <- y[x<=xu[i]]
    partRSS <- sum((partR-mean(partR))^2)
    partLSS <- sum((partL-mean(partL))^2)
    ss[i] <- partRSS + partLSS
  }
  xnode <- xu[which.min(ss)]
  minss <- min(ss)
  pR <- mean(y[x>xnode])
  pL <- mean(y[x<=xnode])
  return(list(xnode=xnode,yR=pR,yL=pL))
}

# Can Boosting Learn the Sine Wave!
x <- seq(0,2*pi,pi/20)
y <- sin(x)
windows(height=300,width=100)
par(mfrow=c(3,1))
plot(x,y,"l",col="red",main="Oh My Waves!")

first_split <- getNode(x,y)
first_split
segments(x0=min(x),y0=first_split$yL,
         x1=first_split$xnode,y1=first_split$yL)
segments(x0=first_split$xnode,y0=first_split$yR,
         x1=max(x),y1=first_split$yR)
yfit1 <- ifelse(x<first_split$xnode,first_split$yL,first_split$yR)
GBFit <- yfit1
segments(x0=x,x1=x,y0=y,y1=yfit1)
first_residuals <- y-yfit1
summary(first_residuals)

second_split <- getNode(x,first_residuals)
plot(x,first_residuals,"l",col="red",main="The Second Wave!")
segments(x0=min(x),y0=second_split$yL,
         x1=second_split$xnode,y1=second_split$yL)
segments(x0=second_split$xnode,y0=second_split$yR,
         x1=max(x),y1=second_split$yR)
yfit2 <- ifelse(x<second_split$xnode,second_split$yL,second_split$yR)
GBFit <- GBFit+yfit2
segments(x0=x,x1=x,y0=first_residuals,y1=yfit2)
second_residuals <- first_residuals-yfit2
summary(second_residuals)

third_split <- getNode(x,second_residuals)
plot(x,second_residuals,"l",col="red",main="The Third Wave!")
segments(x0=min(x),y0=third_split$yL,
         x1=third_split$xnode,y1=third_split$yL)
segments(x0=third_split$xnode,y0=third_split$yR,
         x1=max(x),y1=third_split$yR)
yfit3 <- ifelse(x<third_split$xnode,third_split$yL,third_split$yR)
GBFit <- GBFit+yfit3
segments(x0=x,x1=x,y0=second_residuals,y1=yfit3)
third_residuals <- second_residuals-yfit3
summary(third_residuals)

pdf("Sine_Wave_25_Iterations.pdf")
curr_residuals <- third_residuals
for(j in 4:25){
  jth_split <- getNode(x,curr_residuals)
  plot(x,curr_residuals,"l",col="red",main=paste0(c("The ", j, "th Wave!")))
  segments(x0=min(x),y0=jth_split$yL,
           x1=jth_split$xnode,y1=jth_split$yL)
  segments(x0=jth_split$xnode,y0=jth_split$yR,
           x1=max(x),y1=jth_split$yR)
  yfit_next <- ifelse(x<jth_split$xnode,jth_split$yL,jth_split$yR)
  GBFit <- GBFit+yfit_next
  segments(x0=x,x1=x,y0=curr_residuals,y1=yfit_next)
  curr_residuals <- curr_residuals-yfit_next
}
dev.off()
summary(curr_residuals)

plot(y,GBFit,xlab="True Y",ylab="Gradient Boosting Fit")


# Using the rpart function

# Gradiend Boosting Using the Squared-error Loss Function
GB_SqEL <- function(y,X,depth,iter,shrinkage){
  curr_res <- y
  GB_Hat <- data.frame(matrix(0,nrow=length(y),ncol=iter))
  fit <- y*0
  for(i in 1:iter){
    tdf <- cbind(curr_res,X)
    tpart <- rpart(curr_res~.,data=tdf,maxdepth=depth)
    gb_tilda <- predict(tpart)
    gb_hat <- shrinkage*gb_tilda
    fit <- fit+gb_hat
    curr_res <- curr_res-gb_hat
    GB_Hat[,i] <- fit
  }
  return(list(GB_Hat = GB_Hat))
}

als <- read.table("http://web.stanford.edu/~hastie/CASI_files/DATA/ALS.txt",header=TRUE)
# OR 
als <- read.table("../Data/ALS.txt",header=TRUE)
alst <- als[als$testset==FALSE,-1]

temp <- GB_SqEL(y=alst$dFRS,X=alst[,-1],depth=4,
                iter=500,shrinkage = 0.02)
MSE_Train <- 0
for(i in 1:500){
  MSE_Train[i] <- mean(temp$GB_Hat[,i]-alst$dFRS)^2
}
plot.ts(MSE_Train)


# The GBM Package

toy <- data.frame(x1=x1,x2=x2,y=y)
toy$y <- as.factor(toy$y)

toy_gbm <- gbm(y~.,distribution = "bernoulli",data=toy,
               n.trees=10)

