#not oversampled data




df_over = data
data=df1

data$age.z = (data$age-mean(data$age))/sd(data$age)
data$bmi.z = (data$bmi-mean(data$bmi))/sd(data$bmi)
data$glucose.z = (data$avg_glucose_level-mean(data$avg_glucose_level))/sd(data$avg_glucose_level)

smp_size<-floor(0.7*nrow(data))
set.seed(1024)
trainingdata <- sample(seq_len(nrow(data)),size=smp_size)
training<-data[trainingdata,]
testing<-data[-trainingdata,]

prop.table(table(training$stroke))
prop.table(table(testing$stroke))







library(rpart)

cartfit=rpart(stroke~ age.z + hypertension + heart_disease + ever_married +  Residence_type
              + glucose.z + bmi.z+ gender+ work_type + smoking_status, data=training, method="class")
cartfit

#install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(cartfit, main ="Classification Tree")


df_tree_o = data

data = df_over
data=df_tree_over


cartfit=rpart(stroke~ age.z + hypertension + heart_disease + ever_married +  Residence_type
              + glucose.z + bmi.z+ gender+ work_type + smoking_status, data=training, method="class")
cartfit
rpart.plot(cartfit, main ="Classification Tree")

pred = predict(cartfit, type="class")
table(pred)
prop.table(table(pred))

pred_test = predict(cartfit, type="class",newdata = testing)
table(pred_test)

table1<-table(predicted = pred, actual =training$stroke )
table1
sumtable<-addmargins(table1,FUN=sum)
sumtable

p<-predict(cartfit,data)
p


prediction<-ifelse(p>0.5, 1,0)
prediction
head(prediction)

summary(cartfit)

acc_tree <- function(pred,pred_test) {
  out = c()
  
  ## Classification table

  tab_train <- table(pred,actual = training$stroke)
  out$sumtab_train<- addmargins(tab_train,FUN=sum)
  
  tab_tes <-table(pred_test,actual = testing$stroke)
  out$sumtab_test<- addmargins(tab_tes,FUN=sum)
  
  TAP <- sum(tab_tes[,2])   #Total actual positives
  TAN <- sum(tab_tes[,1])   # Total actual negatives
  
  TP <- out$sumtab_test[2,2]
  TN <- out$sumtab_test[1,1]
  FP <- out$sumtab_test[2,1]
  FN <- out$sumtab_test[1,2]
  out$TPR = TP/TAP    # Sensitivity or recall ## ability to correctly classify 
  out$FPR = FP/TAN    
  out$TNR = TN/TAN   # Specificity
  out$FNR = FN/TAP
  out$accuracy = (TP+TN)/(TAN+TAP)
  out$miss_classification_error = 1-out$accuracy
  
  out$precision = TP/(TP+FP)
  # conditional probability of being positive when predicted positive
  out$specificity <- TN/TAN
  out$f_score = TP/(TP+0.5*(FP+FN))
  return(out)
}

acc_tree(cartfit,pred,pred_test)


x_training = training[,c(1,3,4,5,6,7,10,27,29)]
x_training

y_training = training$stroke

x_test = testing[,c(1,3,4,5,6,7,10,27,29)]
y_test = testing$stroke

#install.packages('C50')
#library(C50)
tree_mod <- C5.0(x = x_training , y = y_training)
tree_mod
summary(tree_mod)
plot(tree_mod)



pred = predict.C5.0(tree_mod,type = 'class',newdata=x_training)
pred_test = predict.C5.0(tree_mod,newdata = x_test,type='class')

acc_tree(pred,pred_test)



df_tree_over = data
data = df_tree_over


pruned_tree <- C5.0(x = x_training , y = y_training,control = C5.0Control(CF = 0.01))
summary(pruned_tree)
plot(pruned_tree)

pred = predict.C5.0(pruned_tree,type = 'class',newdata=x_training)
pred_test = predict.C5.0(pruned_tree,newdata = x_test,type='class')

acc_tree(pred,pred_test)


library(randomForest)
library(tree)

train = training[,c(1,3,4,5,6,7,10,27,29,29,11)]
test = testing[,c(1,3,4,5,6,7,10,27,29,29,11)]

treemod= tree(stroke~., data=train)
summary(treemod)

plot (treemod)
text(treemod,pretty=0)
p1<-predict(treemod,data)
prediction1<-ifelse(p1>0.5, 1,0) 

# on original
Ranfor= randomForest(stroke~., data=train)
print(Ranfor) 
importance(Ranfor)
pred<-predict(Ranfor,train)
pred_test <-predict(Ranfor,test)


acc_tree(pred,pred_test)


# on over

Ranfor= randomForest(stroke~glucose.z+age.z+smoking_status+work_type, data=train)
print(Ranfor) 
importance(Ranfor)
pred<-predict(Ranfor,train)
pred_test <-predict(Ranfor,test)


acc_tree(pred,pred_test)





tab_test = table(p1,actual = testing$income)
tab_test
TAP <- sum(tab_test[,2])   #Total actual positives
TAN <- sum(tab_test[,1])   # Total actual negatives