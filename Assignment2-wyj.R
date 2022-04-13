##PREDICT purchase 

rm(list=ls())

dat <- read.csv("Caravan.csv")
dim(dat)
library(psych)
describe(dat)[,1:9]
head(dat)

#########################################
#	Define purchase (purchase (1) vs. non-purchase (0))
#########################################
table(dat$Purchase)
dat$Purchase <- I(dat$Purchase == 'Yes') * 1
describe(dat$Purchase)[,1:9]
head(dat$Purchase)
#dat$purchase <- ifelse(dat$purchase == 'Yes',1,0)

#########################################
#	Split dataset
#########################################

set.seed(42)

test <- dat[sample(nrow(dat), 1000, replace = FALSE), ]
dim(test)
train <- dat[unlist(-test),]
dim(train)

sample_size <- ceiling(1000/5822*nrow(dat))
train_ind <- sample(seq_len(nrow(dat)),size = sample_size)
test <- dat[train_ind, ]
train <- dat[-train_ind, ]

#########################################
#	logistic regression
#########################################

glm <- glm(Purchase ~ ., family="binomial", data=train)
summary(glm)
yhat_glm <- predict(glm, type="response")

#determine the confusion matrix for a 50% threshold 
table(train$Purchase, (yhat_glm >0.5))

#TRUE POSITIVE RATE, SENSITIVITY
TPR <- function(y,yhat)  { sum(y==1 & yhat==1) / sum(y==1) }
TPR(train$Purchase, (yhat_glm >0.5))

#TRUE NEGATIVE RATE, SPECIFICITY
TNR <- function(y,yhat)  { sum(y==0 & yhat==0) / sum(y==0) }
TNR(train$Purchase, (yhat_glm >0.5))

test$yhat.glm <- predict(glm, test, type="response")
table(test$Purchase, (test$yhat.glm > 0.5))
TPR(test$Purchase, (test$yhat.glm > 0.5))
TNR(test$Purchase, (test$yhat.glm > 0.5))
