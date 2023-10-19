#linear regression
library(caTools)
data <- data.frame(
  Years_Exp = c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7),
  Salary = c(39343.00, 46205.00, 37731.00, 43525.00,
             39891.00, 56642.00, 60150.00, 54445.00, 64445.00, 57189.00)
)
plot(data$Years_Exp, data$Salary,pch=19)
split=sample.split(data$Salary)
train=subset(data, split == TRUE)
test=subset(data, split == FALSE)
md= lm(formula = Salary ~ Years_Exp,data = train)
coef(md)
ypred = predict(md, newdata = test)
library(ggplot2)
ggplot() + 
  geom_point(aes(x = train$Years_Ex, y = train$Salary),colour = 'red') +
  geom_line(aes(x = train$Years_Ex,y = predict(md, newdata = train)),colour = 
              'blue')



#LOGISTIC REGRESSION
#logistic regression
install.packages("caTools")
library(caTools)
head(mtcars)
set.seed(300)
split<-sample.split(mtcars$vs)
train<-subset(mtcars,split==TRUE)
test<-subset(mtcars,split==FALSE)
md<-glm(vs~hp+wt,data=mtcars,family = binomial())
md
pred<-predict(md,test)
pred<-ifelse(pred>0.5,1,0)
table(pred,test$vs)
install.packages("ROCR")
library(ROCR)
roc<-performance(prediction(pred,test$vs),measure = 'tpr',x.measure = 'fpr')
plot(roc)

auc <- performance(prediction(pred,test$vs), measure = "auc")
auc <- auc@y.values[[1]]
auc
abline(a = 0, b = 1)
auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)

