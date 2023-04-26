
heart <- read.csv("heart.csv")

# categorical variable은 factorize 
heart$sex <- as.factor(heart$sex)
heart$cp <- as.factor(heart$cp)
heart$fbs <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exang <- as.factor(heart$exang)
heart$slope <- as.factor(heart$slope)
heart$ca <- as.factor(heart$ca)
heart$thal <- as.factor(heart$thal)
heart$target <- as.factor(heart$target)

# making dummy columns (target 제외한 categorical variables)
library(tidytable)

heart_dummies <- (heart %>% 
  get_dummies.(cols = c(sex, cp, fbs, restecg, exang, slope, ca, thal)))
names(heart_dummies) # 생성된 dummy column 확인 

# drop remained original factor columns  
library(dplyr)
heart_dummies <-
  select(heart_dummies, -c(sex, cp, fbs, restecg, exang, slope, ca, thal))


#########################################################
# No partitioning 
heart0 <- heart_dummies

heart0x <- select(heart0, -target)
heart0y <- select(heart0, target)

# Scaling 
max1 = apply(heart0x, 2, max) 
min1 = apply(heart0x, 2, min)

gdat = scale(heart0x, center = min1, scale = max1 - min1) #Standaization
gdat = as.data.frame(cbind(heart0x, heart0y))

## Model fitting
heart0 = gdat 

fit = glm(target ~., data = heart0, family = binomial(link = "logit"))
summary(fit)

fit2 = step(fit, direction = "both")
fit2$anova
summary(fit2)


## Predicting

fit2.pred = predict(fit2, newdata = heart0, type = "response") 

cutoff = 0.5
fit2.yhat = ifelse(fit2.pred <= cutoff, 0, 1)

ctable = table(heart0$target, fit2.yhat,  dnn = c("Actual", "Predicted"))  
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

fit.pred = predict(fit, newdata =  heart0, type = "response") 
pred = prediction(fit.pred, heart0$target)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend("bottomright", legend = c("Regression","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

#########################################################
# divide train set and test set 
set.seed(123)
V = 2
n =  NROW(heart0)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
heart0.train = heart0[ ii,]
heart0.test  = heart0[-ii,]

## Model fitting

fit = glm(target ~., data = heart0.train, family = binomial(link = "logit"))
summary(fit)

fit2 = step(fit, direction = "both")
fit2$anova
summary(fit2)


## Predicting

fit2.pred = predict(fit2, newdata =  heart0.test, type = "response") 

cutoff = 0.5
fit2.yhat = ifelse(fit2.pred <= cutoff, 0, 1)

ctable = table(heart0.test$target, fit2.yhat,  dnn = c("Actual", "Predicted")) 
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

fit.pred = predict(fit, newdata = heart0.train, type = "response") 
pred = prediction(fit.pred, heart0.train$target)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=0.5, "bottomright", legend = c("Regression","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


fit.pred = predict(fit, newdata = heart0.test, type = "response") 
pred = prediction(fit.pred, heart0.test$target)

perf = performance(pred, "tpr","fpr")
plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=0.5, "bottomright", legend = c("Regression","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

