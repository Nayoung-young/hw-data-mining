
library(e1071)  #install.packages("e1071")


#heart <- read.csv('heart.csv')
heart <- read.csv('/kaggle/input/heart-disease-dataset/heart.csv')


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
heart_dummies <-
  select(heart_dummies, -c(sex, cp, fbs, restecg, exang, slope, ca, thal))

heart0 = heart_dummies

## Model fitting

fit = svm(y ~., data = heart0, kernel="radial", cost=1, decision.values=TRUE, probability=TRUE)
#fit = svm(y ~., data = heart0, kernel="polynomial", cost=1, decision.values=TRUE, probability=TRUE)
#fit = svm(y ~., data = heart0, kernel="linear", cost=1, decision.values=TRUE, probability=TRUE)
summary(fit)


## Predicting

fit.pred = predict(fit, newdata = heart0, decision.values=TRUE, probability=TRUE)
pred = attributes(fit.pred)$decision.values
#pred = attributes(fit.pred)$probability[,1] 


cutoff = 0.5
fit.yhat = ifelse(pred <= cutoff, 0, 1)

ctable = table(heart0$target, fit.yhat,  dnn = c("Actual", "Predicted"))  
ctable


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

#install.packages("ROCR")
library(ROCR)

fit.pred = predict(fit, newdata = heart0,  decision.values=TRUE, probability=TRUE)
pred = attributes(fit.pred)$decision.values
#pred = attributes(fit.pred)$probabilities[,1] 
pred = prediction(pred, heart0$target)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex = 0.5, "bottomright", legend = c("SVM","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


###########################################
# Computing the test error by paritioning


## Data partitioning

set.seed(123)
V = 2
n =  NROW(heart0)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
heart0.train = heart0[ ii,]
heart0.test  = heart0[-ii,]


## Model fitting

fit = svm(target ~., data = heart0.train, kernel="radial", cost=1, decision.values=TRUE, probability=TRUE)
#fit = svm(target ~., data = heart0.train, kernel="polynomial", cost=1, decision.values=TRUE, probability=TRUE)
#fit = svm(target ~., data = heart0.train, kernel="linear", cost=1, decision.values=TRUE, probability=TRUE)

summary(fit)


## Predicting

fit.pred = predict(fit, newdata = heart0.test, decision.values=TRUE, probability=TRUE)
pred = attributes(fit.pred)$decision.values
#pred = attributes(fit.pred)$probabilities[,1]

cutoff = 0.5
fit.yhat = ifelse(pred <= cutoff, 0, 1)

ctable = table(heart0.test$target, fit.yhat,  dnn = c("Actual", "Predicted"))  
ctable


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable) # Misclassification Rate
miss.err

pred.acc = 1 - miss.err #Prediction Accuracy
pred.acc  

diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


## ROC and AUC

library(ROCR)
par(mfrow = c(1,2))

fit.pred = predict(fit, newdata = heart0.train,   decision.values=TRUE, probability=TRUE)
pred = attributes(fit.pred)$decision.values
#pred = attributes(fit.pred)$probabilities[,1]
pred = prediction(pred, heart0.train$target)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=0.5, "bottomright", legend = c("SVM_linear","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


fit.pred = predict(fit, newdata = heart0.test, decision.values=TRUE, probability=TRUE)
pred = attributes(fit.pred)$decision.values
#pred = attributes(fit.pred)$probabilities[,1]
pred = prediction(pred, heart0.test$target)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=0.5, "bottomright", legend = c("SVM_linear","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


