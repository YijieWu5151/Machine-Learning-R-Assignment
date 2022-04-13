rm(list=ls())

tel_data <- read.csv("tel.csv",encoding = "UTF-8")
head(tel_data)
summary(tel_data)

#----------------------regression model test1-----------------------------
#every variable
tel_reg1 <- lm(X.U.FEFF.Hours ~ ., data = tel_data) #run the regression
summary(tel_reg1) #summary of the regression

preds1 <- predict(tel_reg1)
eps1 <- tel_data$X.U.FEFF.Hours - preds1
par(mfrow=c(1,2))
hist(eps1)
qqnorm(eps1)
qqline(eps1)

#----------------------regression model test2-----------------------------
#Day--> breakdown (friday)
tel_data$Friday <- ifelse(tel_data$Day==5,1,0)
head(tel_data)
summary(tel_data)

tel_reg2 <- lm(X.U.FEFF.Hours ~ ., data = tel_data) #run the regression
summary(tel_reg2) #summary of the regression


#----------------------regression model test3-----------------------------
#delete自相关的变量
cor(tel_data$Day,tel_data$Friday)
tel_reg3 <- lm(X.U.FEFF.Hours ~ Friday + SOB + Hot + RWT, data = tel_data) #run the regression
summary(tel_reg3) #summary of the regression


preds3 <- predict(tel_reg3)
eps3 <- tel_data$X.U.FEFF.Hours - preds3
par(mfrow=c(1,2))
hist(eps3)
qqnorm(eps3)
qqline(eps3)
