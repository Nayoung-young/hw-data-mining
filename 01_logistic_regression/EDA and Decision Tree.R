
heart <- read.csv("heart.csv")

## 요약통계 
str(heart)
summary(heart)

## numerical value, categorical value 분리 / factorial variable은 factorize 
## 확인: aggregate(heart$ca, by = list(heart$ca), FUN = length) 
num_vars = subset(heart, select = c(age, trestbps, chol, thalach, oldpeak))
cat_vars = subset(heart, select = c(sex, cp, fbs, restecg, exang, slope, ca, thal, target))

heart$sex <- as.factor(heart$sex)
heart$cp <- as.factor(heart$cp)
heart$fbs <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exang <- as.factor(heart$exang)
heart$slope <- as.factor(heart$slope)
heart$ca <- as.factor(heart$ca)
heart$thal <- as.factor(heart$thal)
heart$target <- as.factor(heart$target)

#####################################

# Exploratory Data Analysis (EDA)

#####################################

## correlation plot 
library(corrplot) #install.packages("corrplot")
corrplot.mixed(cor(num_vars),
               upper = "square", 
               lower = "number", 
               addgrid.col= "black",
               tl.col = "black",
               number.cex = 0.5, # font size of number 
               tl.cex = 0.5) # font size of label text 

library(tidyverse) #install.packages("tidyverse")
library(psych) #install.packages("psych")
library(colorspace) #install.packages("colorspace")

pairs.panels(num_vars) # pairs(num_vars)

## EDA - target 0(or 1) feature barplot
target0 = subset(heart, subset = (target == 0))
target1 = subset(heart, subset = (target == 1))

library(ggplot2)

par(mfrow=c(2,3)) # numeric var histogram 

hist(target0$age, freq=F, main = "age",
     xlab = "age (target = 0)", ylab = "Density")
hist(target0$trestbps, freq=F, main = "trestbps",
     xlab = "trestbps (target = 0)", ylab = "Density")
hist(target0$chol, freq=F, main = "chol",
     xlab = "chol (target = 0)", ylab = "Density")
hist(target0$thalach, freq=F, main = "thalach",
     xlab = "thalach (target = 0)", ylab = "Density")
hist(target0$oldpeak, freq=F, main = "oldpeak",
     xlab = "oldpeak (target = 0)", ylab = "Density")

par(mfrow=c(2,3)) # numeric var histogram 

hist(target1$age, freq=F, main = "age",
     xlab = "age (target = 1)", ylab = "Density")
hist(target1$trestbps, freq=F, main = "trestbps",
     xlab = "trestbps (target = 1)", ylab = "Density")
hist(target1$chol, freq=F, main = "chol",
     xlab = "chol (target = 1)", ylab = "Density")
hist(target1$thalach, freq=F, main = "thalach",
     xlab = "thalach (target = 1)", ylab = "Density")
hist(target1$oldpeak, freq=F, main = "oldpeak",
     xlab = "oldpeak (target = 1)", ylab = "Density")

target0_cat = subset(cat_vars, subset = (target == 0))
target1_cat = subset(cat_vars, subset = (target == 1))

# categorical var barplot 
# 두 그룹의 count 다르므로 %로 값 바꿀 필요성 있음 

library(gridExtra)

# sex, cp, fbs, restecg, exang, slope, ca, thal, target
a = ggplot(heart, aes(x=factor(sex), fill=target))+
  geom_bar(stat="count", width=0.7, position=position_dodge()) +
  labs(x="sex", y = "count") + 
  theme(legend.title = element_blank()) + 
  theme(legend.position = 'none')  

b = ggplot(heart, aes(x=factor(cp), fill=target))+
  geom_bar(stat="count", width=0.7, position=position_dodge()) +
  labs(x="cp", y = "count")+
  theme(legend.title = element_blank()) + 
  theme(legend.position = 'none') 

c = ggplot(heart, aes(x=factor(fbs), fill=target))+
  geom_bar(stat="count", width=0.7, position=position_dodge()) +
  labs(x="fbs", y = "count")+
  theme(legend.title = element_blank()) + 
  theme(legend.position = 'none')  

d = ggplot(heart, aes(x=factor(restecg), fill=target))+
  geom_bar(stat="count", width=0.7, position=position_dodge()) +
  labs(x="restecg", y = "count")+
  theme(legend.title = element_blank()) + 
  theme(legend.position = 'none')  

e = ggplot(heart, aes(x=factor(exang), fill=target))+
  geom_bar(stat="count", width=0.7, position=position_dodge()) +
  labs(x="exang", y = "count")+
  theme(legend.title = element_blank()) + 
  theme(legend.position = 'none')  

f = ggplot(heart, aes(x=factor(slope), fill=target))+
  geom_bar(stat="count", width=0.7, position=position_dodge()) +
  labs(x="slope", y = "count")+
  theme(legend.title = element_blank()) + 
  theme(legend.position = 'none')  

g = ggplot(heart, aes(x=factor(ca), fill=target))+
  geom_bar(stat="count", width=0.7, position=position_dodge()) +
  labs(x="ca", y = "count")+
  theme(legend.title = element_blank()) + 
  theme(legend.position = 'none')  

h = ggplot(heart, aes(x=factor(thal), fill=target))+
  geom_bar(stat="count", width=0.7, position=position_dodge()) +
  labs(x="thal", y = "count")+
  theme(legend.title = element_blank()) + 
  theme(legend.position = 'none')  

i = ggplot(heart, aes(x=factor(target), fill=target))+
  geom_bar(stat="count", width=0.7, position=position_dodge()) +
  labs(x="target", y = "count")

grid.arrange(a,b,c,d,e,f,g,h, i, nrow=3, ncol=3)




#####################################

# Decision Tree 

#####################################

library(rpart) #install.packages("rpart")
library(rpart.plot)

## 1) No partitioning 
set.seed(1)
fit = rpart(target ~., data=heart, method="class")
rpart.plot(fit)

fit2 = rpart(target ~., data=heart, method="class", control = rpart.control(xval=10, cp=0))
rpart.plot(fit2)

# prune 
tmp = printcp(fit2)
k = which.min(tmp[,"xerror"])
err = tmp[k,"xerror"]; se = tmp[k,"xstd"]; k = which(tmp[,"xerror"] <= err+se)[1]
cp.tmp = tmp[k,"CP"]
fit.pruned = prune(fit2, cp=cp.tmp)
rpart.plot(fit.pruned)

# predicting  
cutoff = 0.5
pred = predict(fit.pruned, newdata=heart, type="prob") #prediction
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(heart$target, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

# evaluating 
miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity

# ROC and AUC
library(ROCR) #install.packages("ROCR")

pred2 = predict(fit.pruned, newdata=heart, type="prob") #prediction
pred = prediction(pred2[,2], heart$target)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend("bottomright", legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC

## 2) Partitioning 
set.seed(12)
V = 2
n =  NROW(heart)
id = sample(1:V, n, prob = c(0.7,0.3), replace = T) # Partitioning 7:3
ii = which(id==1)
heart.train = heart[ii,]
heart.test  = heart[-ii,]


fit = rpart(target ~., data=heart.train, method="class", control = rpart.control(xval=10, cp=0))
#fit
#summary(fit)
rpart.plot(fit)

# pruning
tmp = printcp(fit)
k = which.min(tmp[,"xerror"])
cp.tmp = tmp[k,"CP"]
fit.pruned = prune(fit, cp=cp.tmp)
rpart.plot(fit.pruned)

# predicting  
cutoff = 0.5
pred = predict(fit.pruned, newdata=heart.test, type="prob") #prediction
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(heart.test$target, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

# evaluating 
miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity

# ROC and AUC
library(ROCR)
par(mfrow = c(1,2))

pred2 = predict(fit.pruned, newdata=heart.train, type="prob") #prediction
pred = prediction(pred2[,2], heart.train$target)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Train)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=0.5, "bottomright", legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC


pred2 = predict(fit.pruned, newdata=heart.test, type="prob") #prediction
pred = prediction(pred2[,2], heart.test$target)
perf = performance(pred, "tpr","fpr")

plot(perf, col = 4, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve (Test)") #ROC
lines(x = c(0,1), y = c(0,1), col = 2, lty = 2, lwd = 2)
legend(cex=0.5, "bottomright", legend = c("Tree","Random"), col = c(4,2), lty = c(1,2), lwd = 2)

performance(pred, "auc")@y.values #AUC





