rm(list = ls())

dat <- read.csv("train.csv")
dim(dat)
dat[1:2,]
dat <- dat[,-1]
dim(dat)

#Preparing the data
loss <- dat$loss
hist(loss)
hist(log(loss))
qqnorm(log(loss))
qqline(log(loss), col="blue", lwd=3)

quantile(loss)
quantile(loss, p=seq(0,1,.01))
sum(loss < 100)
loss[loss<100]

#disregard losses that are very small and keep only records with loss greater or equal to $100
dat <- dat[dat$loss>=100,]
dim(dat)
loss <- dat$loss
hist(log(loss))
qqnorm(log(loss))
qqline(log(loss), col="blue", lwd=3)
#Looks better.

#For simplicity, we consider a training data set that only consists of 20% of the data.(not a typical choice in applications)
#so as to keep runtimes manageable
set.seed(652)
trn <- runif(nrow(dat)) < 0.2
table(trn)
train <- dat[trn==TRUE,]
test <- dat[trn==FALSE,]
dim(train); dim(test)

#Predictive Modeling
r0 <- lm(loss~., data=train)
summary(r0)

X <- data.frame(train[,1:130])
Y <- train$loss
X.tst <- data.frame(test[,1:130])
Y.tst <- test$loss

do.RMSE.trn <- function(yhat)  sqrt( mean( (Y-yhat)^2 ) )
do.RMSE.tst <- function(yhat)  sqrt( mean( (Y.tst-yhat)^2 ) )

RMSE.trn_OLS <- do.RMSE.trn(predict(r0, data = train))
RMSE.tst_OLS <- do.RMSE.tst(predict(r0, data = test))
RMSE.trn_OLS^2; RMSE.tst_OLS^2

###########LASSO logistic regression#################
library(glmnet)

#set data matrix in glmnet(not data frame as before)
X_train <- data.matrix(train[,1:130])
Y_train <- train[,131]
X_test <- data.matrix(test[,1:130])
Y_test <- test[,131]

#run LASSO regression
lasso_mod <- glmnet(X_train, Y_train, family = 'gaussian', alpha = 1, standardize = TRUE, nlambda = 10)

#visualize
plot(lasso_mod, lwd=3, xvar='lambda')
#coefficient
coef(lasso_mod)

#in-sample error of different models
mse_train <- colMeans((replicate(10,Y_train)-predict(lasso_mod,X_train))^2)
plot(mse_train,type='o',lwd=3,col='blue',xlab='model complexity')

#out-of-sample error
mse_test <- colMeans((replicate(10,Y_test)-predict(lasso_mod,X_test))^2)
lines(mse_test,type='o',lwd=3,col='red')
mse_test


#compare R^2 to the best selected model from before
cor(predict(ro,X_test),Y_test)^2
cor(predict(lasso_mod,X_test),Y_test)^2

#cross validation
cv_lasso <- cv.glmnet(X_train, Y_train, alpha = 1, family = 'gaussian', k = 5)
#visualize
plot(cv_lasso)
lambda_lasso <- cv_lasso$lambda.min
lambda_lasso


lasso_best <- glmnet(X_train, Y_train, family = 'gaussian', alpha = 1, lambda = lambda_lasso, standardize = TRUE)
cor(predict(lasso_best,X_test),Y_test)^2
cor(predict(r0,X.tst),Y.tst)^2
