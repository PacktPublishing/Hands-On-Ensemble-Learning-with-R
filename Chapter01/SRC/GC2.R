library(RSADBE)
library(data.table)
library(plyr)
data(GC)
GC2 <- GC
GC2$checking <- as.factor(GC2$checking)
GC2$checking <- revalue(GC2$checking, c("1" = "< 0 DM",
                                        "2" = "0 <= ... < 200 DM",
                                        "3" = ">= 200 DM", 
                                        "4" = "No Acc"))
GC2$history <- as.factor(GC2$history)
GC2$history <- revalue(GC2$history, c("0" = "All Paid",
                                      "1" = "Bank paid",
                                      "2" = "Existing paid",
                                      "3" = "Delayed", 
                                      "4" = "Dues Remain"))
GC2$purpose <- as.factor(GC2$purpose)
GC2$purpose <- revalue(GC2$purpose,c( "0" = "New Car", 
                                      "1" = "Old Car", 
                                      "2" = "Furniture",
                                      "3" = "Television", 
                                      "4" = "Appliance", 
                                      "5" = "Repairs",
                                      "6" = "Education", 
                                      "8" = "Retraining", 
                                      "9" = "Business",
                                      "X" = "Others"))
GC2$savings <- as.factor(GC2$savings)
GC2$savings <- revalue(GC2$savings,c( "1" = "< 100 DM ",
                                      "2" = "100-500 DM",
                                      "3" = "500-1000 DM", 
                                      "4" = ">1000 DM",
                                      "5" = "Unknown"))
GC2$employed <- as.factor(GC2$employed)
GC2$employed <- revalue(GC2$employed,c( "1" = "Unemployed", 
                                        "2" = "1 Year", 
                                        "3" = "1-4 Years",
                                        "4" = "4-7 Years", 
                                        "5" = ">7 Years"))
GC2$marital <- as.factor(GC2$marital)
GC2$marital <- revalue(GC2$marital,c( "1" = "Female S", 
                                      "2" = "Female M/D", 
                                      "3" = "Male M/D",
                                      "4" = "Male S"))
GC2$coapp <- as.factor(GC2$coapp)
GC2$coapp <- revalue(GC2$coapp,c( "1" = "None", 
                                  "2" = "Co-app", 
                                  "3" = "Guarantor"))
GC2$property <- as.factor(GC2$property)
GC2$property <- revalue(GC2$property,c( "1" = "Real Estate", 
                                        "2" = "Building society",
                                        "3" = "Others", 
                                        "4" = "Unknown"))
GC2$other <- NULL # because "none" is dominating frequency
GC2$housing <- as.factor(GC2$housing)
GC2$housing <- revalue(GC2$housing, c( "1" = "Rent", 
                                       "2" = "Own",
                                       "3" = "Free"))
GC2$job <- as.factor(GC2$job)
GC2$job <- revalue(GC2$job, c( "1" = "Unemployed", 
                               "2" = "Unskilled", 
                               "3" = "Skilled",
                               "4" = "Highly Qualified"))
GC2$telephon <- as.factor(GC2$telephon)
GC2$telephon <- revalue(GC2$telephon, c( "1" = "None", 
                                         "2" = "Registered"))
GC2$foreign <- as.factor(GC2$foreign)
GC2$foreign <- revalue(GC2$foreign, c( "1" = "No", 
                                       "2" = "Yes"))