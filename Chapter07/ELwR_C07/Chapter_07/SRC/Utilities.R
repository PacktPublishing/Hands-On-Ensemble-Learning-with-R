

Get_Prob <- function(Logical,Probability){
  return(t(ifelse(Logical,Probability,1-Probability)))
}


Random_Accuracy <- function() {
  accuracy <- runif(9)
  NT <- length(accuracy) 
  APC <- expand.grid(rep(list(c(TRUE,FALSE)),NT)) 
  Elements_Prob <- t(apply(APC,1,Get_Prob,Probability=accuracy))
  Events_Prob <- apply(Elements_Prob,1,prod)
  Majority_Events <- (rowSums(APC)>NT/2)
  return(sum(Events_Prob*Majority_Events))
}

Row_Count_Max <- function(x) names(which.max(table(x)))

headtail <- function(x,n) {
  c(head(x,n),tail(x,n))
  }
