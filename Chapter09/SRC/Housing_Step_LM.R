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
