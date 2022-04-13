wagedata <- read.csv('Wage.csv',header = TRUE)

set.seed(42)
index <- sort(sample(1:nrow(wagedata),round(0.75*nrow(wagedata))))
train <- wagedata[index, ]
test  <- wagedata[-index, ]


library(mgcv)
gam1 <- gam(wage ~ s(year,k=7) + s(age, bs='cr') + education, family=gaussian, data=train)
#s means they are smoothed, 'cr' determines by cross validation

summary(gam1)
plot(gam1,scale=0)

cor(predict(gam1,newdata = data_las_test),y_test)^2