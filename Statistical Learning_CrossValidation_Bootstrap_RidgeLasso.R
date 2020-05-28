library(dplyr)
library(MASS)
library(class)
library(ISLR)
library(caret)

setwd("R")
setwd("Datasets")


########     P1      ###################
######## LDA QDA KNN ####################

#misrate function
mis.rates = function(x,y) sum(x!=y)/length(x)

#read data
df <- read.csv('UKM.csv')

summary(df)
str(df)

set.seed(1025)

# make folds
kfolds <- 6
folds <- rep_len(1:kfolds, 258)
folds <- sample(folds, 258)

k.cv.error = matrix(0, kfolds,4)
colnames(k.cv.error)=c("lda", "qda", "knn5", "knn10")
k.cv.error


for(k in 1:kfolds){
  fold <- which(folds == k)
  
  # split the data train/test  for each fold
  sim.train <- df[-fold, ]
  sim.test <- df[fold, ]
  
  
  # LDA
  lda.fit <- lda(UNS ~ STG + SCG + STR + LPR + PEG, data = sim.train)
  lda.pred.test <- predict(lda.fit, newdata=sim.test)  # predict the test set 
  
  # QDA
  qda.fit <- qda(UNS ~ STG + SCG + STR + LPR + PEG, data=sim.train)
  qda.pred.test <- predict(qda.fit, newdata=sim.test)  # predict the test set 
  
  #KNN 5
  knn.pred.test <- knn(sim.train[,-6], sim.test[,-6], cl=as.factor(sim.train[,6]),  k = 5)
  
  #KNN 10
  knn.pred.test2 <- knn(sim.train[,-6], sim.test[,-6], cl=as.factor(sim.train[,6]),  k = 10)
  
  
  # get test error rates for each fold
  k.cv.error[k,1] = mis.rates(lda.pred.test$class, sim.test$UNS)
  k.cv.error[k,2] = mis.rates(qda.pred.test$class, sim.test$UNS)
  k.cv.error[k,3] = mis.rates(knn.pred.test,sim.test$UNS)
  k.cv.error[k,4] = mis.rates(knn.pred.test2,sim.test$UNS)
  
  
}

k.cv.error

#average of error
(test.mse1=apply(k.cv.error, 2, mean))



#Part 2

df.test <- read.csv('UKMtest.csv')
summary(df.test)

#best method QDA
qda1 <- qda(UNS ~ STG + SCG + STR + LPR + PEG, data=df)
qda.pred <- predict(qda1, df.test)
mis.rates(qda.pred$class, df.test$UNS)



#load data
data <- Carseats
summary(data)
str(data)

attach(data)

# linear model
lm.fit <- lm(Sales ~ Price + US)
lm.fit
summary(lm.fit)

#confidence interval
confint(lm.fit, level =0.95)



########     P2      ###################
######## Bootstrap sampling ############ 

set.seed(1025)
B <- 500
Boot.est.b1 <- rep(0, B)
Boot.est.b2 <- rep(0, B)
Boot.est.b0 <- rep(0, B)
for(b in 1:B){
  index <- sample(1:nrow(data), nrow(data), replace=T)   # sample the data with replacement of the same size
  lm.fit1 = lm(Sales ~ Price + US, data = data, subset = index)   # subset option - choosing the training data (index)
  Boot.est.b0[b] <- lm.fit1$coef[1]
  Boot.est.b1[b] <- lm.fit1$coef[2]
  Boot.est.b2[b] <- lm.fit1$coef[3]
}

lm.fit1

hist(Boot.est.b1)
mean(Boot.est.b1)
sd(Boot.est.b1)

hist(Boot.est.b2)
mean(Boot.est.b2)
sd(Boot.est.b2)

quantile(Boot.est.b1, c(0.05, 0.95))
quantile(Boot.est.b2, c(0.05, 0.95))


########     P3      ###################
######## Ridge & Lasso #################

#read data
df <- College
summary(df)
str(df)

#1
model <- train(Apps ~ ., df, method= 'lm', trControl = trainControl( method= 'cv', number = 5, verboseIter = TRUE)) 
summary(model)
print(model)


#2
# Ridge Regression #


x <- model.matrix(Apps ~., data = df)[,-1] # to take out the (Intercept)
y <- df$Apps

grid <- 10^seq(10, -2, length = 100) # lambda from 10^(10) to 10^(-2)

# Divide data to train set and test set.
set.seed(1101)
train <- sample(1:nrow(df), nrow(df)/2)

mydata.train <- df[train, ]
mydata.test <- df[-train, ]

ridge.fit <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid)

# cross validation to choose optimal lambda
set.seed(1101)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0, nfolds = 10)
plot(cv.out)
best.lambda <- cv.out$lambda.min
best.lambda

# Test MSE associated with the optimal lambda
ridge.pred <- predict(ridge.fit, s = best.lambda, newx = x[-train, ])
mean((ridge.pred - mydata.test$Apps)^2)

##??? how to do CV now with lambda I'm trying
cv.final <- cv.glmnet(x, y, alpha = 0, lambda = best.lambda, nfolds = 5)






#3
# Lasso #

x <- model.matrix(Apps ~., data = df)[,-1] # to take out the (Intercept)
y <- df$Apps

set.seed(1025)
train <- sample(1:nrow(df), nrow(df)/2)

mydata.train <- df[train, ]
mydata.test <- df[-train, ]

grid <- 10^seq(10, -2, length = 100)
lasso.fit <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)  # alpha=1 
lasso.fit

# cross validation to choose optimal lambda
set.seed(1025)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10) # alpha = 1 then Lasso
plot(cv.out)
best.lambda <- cv.out$lambda.min
best.lambda

lasso.pred <- predict(lasso.fit, s = best.lambda, newx = x[-train, ])

mean((lasso.pred - mydata.test$Apps)^2)


# Finally, refit our Lasso on the full data set  


final <- cv.glmnet(x, y, alpha = 1, nfolds = 5 )
predict(final, s = best.lambda)
ccc=predict(final, type = "coefficients", s = best.lambda)[1:17,]
which(ccc!=0)
plot(final)