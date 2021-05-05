install.packages("dplyr")
library(car)
library(corrplot) # plot correlations
library(dplyr) # data aggregates
library(Hmisc) # for correlation test of multiple variables
library(gplots)
library(ggplot2)
library("readxl")



cust_stats <- read_excel(file.choose(), sheet = 2)

str(cust_stats)

colnames(cust_stats) <- c("ID" , "Age" , "Churn" , "CHIScore_Month0" , "CHIScore_0_to_1" , "SupportCases_Month0" ,
                          "SupportCases_0_to_1" , "SP_Month0" , "SP_0_to_1" , "Logins_0_to_1" , "BlogArticles_0_to_1" ,
                          "Views_0_to_1" , "Days_Since_LastLogin_0_to_1") #rename

cust_stats$Churn <- as.factor(cust_stats$Churn)

boxplot( cust_stats$Age ~ cust_stats$Churn, data=cust_stats, main="Churn rate based on age",
         xlab="Churn", ylab="Age",
         col=c("orange", "lightblue4"))

library(ISLR)
set.seed(300)
data_target <-cust_stats[-c(672,354,5203),] #exclude rows with target ID
indx <- sample(2, nrow(data_target), replace = T, prob = c(0.7, 0.3)) #divide into training and test data
train <- data_target[indx == 1, ]
test <- data_target[indx == 2, ]
test <- rbind(test,cust_stats[c(354,672,5203),])
View(test)

logitModel <- glm(Churn ~ ., data = train, family = "binomial") 
#a logistic regression model for target variable churn, including all the other variables in the data set
summary(logitModel) #we see that most variables have a very high p value and hence are not significant. So we will exclude these variables and inclue 
#just Days_Since_LastLogin_0-1,SupportCases_0-1,CHIScore_0-1,CHIScore_Month0,Age,ID


logitModel_new <- glm(Churn ~ ID + Age + CHIScore_Month0 + CHIScore_0_to_1 + Days_Since_LastLogin_0_to_1 + Views_0_to_1 + SupportCases_0_to_1 , data = train, family = "binomial")
summary(logitModel_new) 
#new model with variables that have a high significance

options(scipen = 99)
Pred <- predict(logitModel_new, newdata = test, type = "response") #predicted churn for test data 
Pred

#ROC curve to find cut off point 

Class <- ifelse(Pred >= 0.12, "YES", "NO")
Class

#confusion matrix
cf <- table(Pred>0.12,test$Churn) 
cf

(cf[1] + cf[4])/sum(cf) #accuracy

#Predict for customer 672,354,5203

Pred[672]
Pred[354]
Pred[5203]

d<-sort(Pred, decreasing = TRUE) 
#to obtain 100 customers with highest probabilities of churning
d
head(d,100)









