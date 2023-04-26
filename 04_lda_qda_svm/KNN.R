
library(class) #install.packages("class")
# heart <- read.csv('')
heart <- read.csv('../input/heart-disease-dataset/heart.csv')

heart$sex <- as.factor(heart$sex)
heart$cp <- as.factor(heart$cp)
heart$fbs <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exang <- as.factor(heart$exang)
heart$slope <- as.factor(heart$slope)
heart$ca <- as.factor(heart$ca)
heart$thal <- as.factor(heart$thal)
# heart$target <- as.factor(heart$target)

## Making Dummy columns 
library(tidytable)

heart_dummies <- (heart %>% 
                    get_dummies.(cols = c(sex, cp, fbs, restecg, exang, slope, ca, thal)))
names(heart_dummies) # 생성된 dummy column 확인 

# drop remained original factor columns  
library(dplyr)
heart_dummies <-select(heart_dummies, -c(sex, cp, fbs, restecg, exang, slope, ca, thal))

## Scaling 
heart0 <- heart_dummies

heart0x <- select(heart0, -target)
heart0y <- select(heart0, target)

# Scaling 
max1 = apply(heart0x, 2, max) 
min1 = apply(heart0x, 2, min)

gdat = scale(heart0x, center = min1, scale = max1 - min1) #Standaization
gdat = as.data.frame(cbind(heart0x, heart0y))

heart0 = gdat 

X <- select(heart0, -target)
# y <- select(heart0, target) # error 
y <- heart0[ ,31]

#########################################################
## No partitioning 

## KNN with K=5
fit = knn(train=X, test=X, cl=y, k=5) 

## Predicting
yhat=fit
ctable = table(y, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

## Evaluating
miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


###########################################
# Computing the test error by paritioning

## Data partitioning
set.seed(123)
V = 2
n =  NROW(heart0)

id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
X.train = X[ii,]; X.test  = X[-ii,]
y.train = y[ii];  y.test  = y[-ii]

#knn.5 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=5)
#knn.10 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=10)

knn.5 <- knn(train=X.train, test=X.test, cl=y.train, k=5)
knn.10 <- knn(train=X.train, test=X.test, cl=y.train, k=10)

ctable.5 = table(knn.5, y.test)
ctable.10 = table(knn.10, y.test)

library(caret)

confusionMatrix(ctable.5)
confusionMatrix(ctable.10)

## Optimization 
i=1
k.optm=1
for (i in 1:15){
  knn.mod <- knn(train=X.train, test=X.test, cl=y.train, k=i)
  k.optm[i] <- 100 * sum(y.test == knn.mod)/NROW(y.test)
  k=i
  cat(k,'=',k.optm[i],'
')
}

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy

i=1
k.optm=1
for (i in 1:15){
  knn.mod <- knn(train=X.train, test=X.test, cl=y.train, k=i)
  
  ctable = table(knn.mod, y.test)
  miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
  pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
  
  k.optm[i] <- 100 * pred.acc
  k=i
  cat(k,'=',k.optm[i],'
')
}

#Accuracy plot
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")


###############################################

## 유의미한 value 2개만 
# cp_0, sex_0
# cp_0, sex_0, thal_2, slope_1, ca_2

set.seed(123)
V = 2
n =  NROW(heart0)

id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
X.train = X[ii,]; X.test  = X[-ii,]
y.train = y[ii];  y.test  = y[-ii]

#X.train = X.train[, c("cp_0", "sex_0")]; X.test  = X.test[, c("cp_0", "sex_0")]
#y.train = y[ii];  y.test  = y[-ii]

vars = c("cp_0", "sex_0", "thal_2", "slope_1", "ca_2")
X.train = X.train[, vars]; X.test  = X.test[, vars]
y.train = y[ii];  y.test  = y[-ii]

## Optimization 
i=1
k.optm=1
for (i in 1:15){
  knn.mod <- knn(train=X.train, test=X.test, cl=y.train, k=i)
  k.optm[i] <- 100 * sum(y.test == knn.mod)/NROW(y.test)
  k=i
  cat(k,'=',k.optm[i],'
')
}

#Accuracy plot
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

# 2 axis plotting 
library(tidyverse)
library(tidymodels)
library(plotly)




