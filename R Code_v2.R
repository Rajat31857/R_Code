library(knitr)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(mice)
library(lattice)
library(reshape2)
library(DataExplorer)
library(rpart)
library(rpart.plot)
library(forecast)
library(caret)
library(e1071)
setwd("C:/Users/rm318/OneDrive/Desktop/BA$R")
data <- read.csv("default_of_credit_card.csv", header = TRUE)
head(data)
colnames(data) <- as.character(unlist(data[1,]))
data = data[-1, ]
head(data)
colnames(data)[colnames(data)=="default payment next month"] <- "default_payment"
head(data)
dim(data)
str(data)
data[, 1:25] <- sapply(data[, 1:25], as.character)
data[, 1:25] <- sapply(data[, 1:25], as.numeric)
str(data)
##summary(data)
introduce(data)
count(data, vars = EDUCATION)
count(data, vars = MARRIAGE)
#replace 0's with NAN, replace others too
data$EDUCATION[data$EDUCATION == 0] <- 4
data$EDUCATION[data$EDUCATION == 5] <- 4
data$EDUCATION[data$EDUCATION == 6] <- 4
data$MARRIAGE[data$MARRIAGE == 0] <- 3
count(data, vars = MARRIAGE)
count(data, vars = EDUCATION)
plot_correlation(na.omit(data), maxcat = 5L)
plot_histogram(data)
#deleting columns



data_new <- select(data, -one_of('ID','AGE','BILL_AMT2',
                                 'BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6'))
##deleted--ID
data_n <- select(data, -one_of('ID'))
View(data_n)
head(data_new)
data_new[, 1:17] <- scale(data_new[, 1:17])
#create a list of random number ranging from 1 to number of rows from actual data 
#and 70% of the data into training data  

data2 = sort(sample(nrow(data_new), nrow(data_new)*.6))

#creating training data set by selecting the output row values
train <- data_new[data2,]

#creating test data set by not selecting the output row values
test <- data_new[-data2,]

dim(train)
dim(test)

## fit a logistic regression model with the training dataset
log.model <- glm(default_payment ~., data = train, family = binomial(link = "logit"))
summary(log.model)

#better fit log model
log.model.eff<-glm(default_payment ~ LIMIT_BAL+SEX+MARRIAGE+PAY_0+BILL_AMT1+PAY_AMT1, data = train, family = binomial(link = "logit"))
summary(log.model.eff)

glm(formula = default_payment ~ ., family = binomial(link = "logit"), 
    data = train)

test[1:10,]


log.predictions <- predict(log.model, test, type="response")

## Look at probability output
head(log.predictions, 10)

log.prediction.rd <- ifelse(log.predictions > 0.5, 1, 0)
table(log.prediction.rd)
head(log.prediction.rd, 10)
table(log.prediction.rd, test[,18])
table(test$default_payment)
accuracy <- table(log.prediction.rd, test[,18])
View(accuracy)
sum(diag(accuracy))/sum(accuracy)


#Random Forrest
train.index <- sample(c(1:dim(data_n)[1]), dim(data_n)[1]*0.6)  
train.df <- data_n[train.index, ]
valid.df <- data_n[-train.index, ]
View(valid.df)
train.df$default_payment<-as.factor(train.df$default_payment)
valid.df$default_payment<-as.factor(valid.df$default_payment)
rm<-randomForest(default_payment~.,data=train.df,ntree= 500,mtry = 9, importance = TRUE)
rm
plot(rm)
varImp(rm)
varImpPlot(rm,sort=TRUE)
rm
pred<-predict(valid.df,object = rm)
table(pred)
table(valid.df$default_payment)

#Savings
defaulters <- data[data$default_payment == 1, ]
dim(defaulters)

bill_amt <- sum(defaulters$BILL_AMT1 + defaulters$BILL_AMT2 + defaulters$BILL_AMT3 
    + defaulters$BILL_AMT4 + defaulters$BILL_AMT5 + defaulters$BILL_AMT6)
bill_amt


pay_amt <- sum(defaulters$PAY_AMT1 + defaulters$PAY_AMT2 + defaulters$PAY_AMT3 
               + defaulters$PAY_AMT4 + defaulters$PAY_AMT5 + defaulters$PAY_AMT6)
pay_amt

estimated_loss <- bill_amt - pay_amt
estimated_loss

accuracy = .82

potential_savings = estimated_loss*accuracy
potential_savings


#Visualizations
plot_correlation(na.omit(data), maxcat = 5L)
plot_histogram(data)


#read in data set
cred<- read.csv("default_of_credit_card.csv")
colnames(cred) <- as.character(unlist(cred[1,]))
cred = cred[-1, ]
head(cred)
colnames(cred)[colnames(cred)=="default payment next month"] <- "default_payment"
head(cred)
dim(cred)
str(cred)

#convert SEX to categorical variable$ & cbind to data set
SEX<- as.factor(cred[,3])
cred<-cbind(cred[,-3], SEX)
cred[, 1:25] <- sapply(cred[, 1:25], as.character)
cred[, 1:25] <- sapply(cred[, 1:25], as.numeric)
str(cred)

#create scatter plot of AGE - LIMIT_BAL
graphics::plot(cred$LIMIT_BAL ~ cred$AGE, xlab = "AGE", ylab = "LIMIT_BAL")

#create scatter plot of log AGE - LIMIT_BAL
graphics::plot(cred$LIMIT_BAL ~ cred$AGE, xlab = "AGE", ylab = "LIMIT_BAL", log = 'xy')

#create boxplot of AGE - LIMIT_BAL
boxplot(cred$LIMIT_BAL ~ cred$AGE, xlab = "AGE", ylab = "LIMIT_BAL")

#create boxplot of AGE - log(LIMIT_BAL)
boxplot(cred$LIMIT_BAL ~ cred$AGE, xlab = "AGE", ylab = "LIMIT_BAL", log = 'y')

#create histogram of age & defaulters non-defaulters
variable_names<- c("0" = "Nondefaulters", "1" = "Defaulters")
ggplot(data=cred, aes(x=AGE)) + geom_histogram(binwidth=1, colour="black", fill="white") + 
facet_grid(default_payment ~., labeller = as_labeller(variable_names)) +
labs(title= "Nondefaulters & Defaulters vs. Age", x = "Age", y = "Count")

