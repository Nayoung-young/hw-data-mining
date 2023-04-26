
library(MASS) #install.packages("MASS")

heart <- read.csv('heart.csv')

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

## Scaling은 lda에서 필요 없음 

## LDA/QDA
# 유의한 변수만 사용? - cp_0, sex_0, thal_2
# fit = lda(target ~., data=heart0) #LDA
fit = lda(target ~ cp_0+sex_0+thal_2, data=heart0)
fit 
#fit = qda(y ~., data=german) #QDA

## Predicting

cutoff = 0.5
pred = predict(fit, newdata=heart0)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(heart0$target, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity

## ROC and AUC

#install.packages("ROCR")
library(ROCR)
pred2 = predict(fit, newdata=heart0)$posterior
pred = prediction(pred2[,2], heart0$target)
perf = performance(pred, "tpr","fpr")

par(mfrow=c(1,1))
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex= 0.5, "bottomright", legend = c("LDA/QDA","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

###########################################
# Computing the test error by paritioning

## Data partitioning
set.seed(123)
V = 2
n =  NROW(heart0)

id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)

heart0.train = heart0[ii,]
heart0.test  = heart0[-ii,]


## LDA/QDA
# 유의한 변수만 사용? - cp_0, sex_0, thal_2, slope_1, ca_2

# fit = lda(target ~., data=heart0) #LDA
#fit = lda(target ~ cp_0+sex_0+thal_2, data=heart0.train)
fit = lda(target ~ cp_0+sex_0+thal_2+slope_1+ca_2, data=heart0.train)
#fit = qda(target ~ cp_0+sex_0+thal_2, data=heart0.train)

fit 
#fit = qda(y ~., data=german) #QDA

## Predicting

cutoff = 0.5
pred = predict(fit, newdata=heart0.test)$posterior
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(heart0.test$target, yhat, dnn=c("Actual", "Predicted")); ctable #classification table


## Evaluating

miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity

## ROC and AUC

library(ROCR)
par(mfrow = c(1,2))

pred2 = predict(fit, newdata=heart0.train)$posterior
pred = prediction(pred2[,2], heart0.train$target)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend("bottomright", cex=0.5, legend = c("LDA/QDA","Random"), col = c(4,2), lty = c(1,2), lwd = 2)


performance(pred, "auc")@y.values #AUC


pred2 = predict(fit, newdata=heart0.test)$posterior
pred = prediction(pred2[,2], heart0.test$target)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend("bottomright", cex=0.5, legend = c("LDA/QDA","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

## EDA 
par(mfrow = c(1,2))
fit = lda(target ~ cp_0+sex_0+thal_2+slope_1+ca_2, data=heart0.train)
pred1 = predict(fit, newdata=heart0.test)$posterior
plot(pred1[,2])

fit = lda(target ~ cp_0+sex_0+thal_2, data=heart0.train)
pred2 = predict(fit, newdata=heart0.test)$posterior
plot(pred2[,2])
