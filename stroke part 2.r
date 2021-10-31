library(mltools)
library(data.table)

workt <- one_hot(as.data.table(data$work_type))
head(workt)
dim(workt)

names(workt)<-levels(data$work_type)

data$children <- workt$children
data$govtjob = workt$Govt_job
data$neverworked = workt$Never_worked
data$private = workt$Private
data$selfemp = workt$`Self-employed`

str(data)

smoke <- one_hot(as.data.table(data$smoking_status))
head(smoke)
data$formerlysm <- smoke$`V1_formerly smoked`
data$neversm<- smoke$`V1_never smoked`
data$smokes <- smoke$V1_smokes
data$unknownsm <- smoke$V1_Unknown

gender <- one_hot(as.data.table(data$gender))
head(gender)
data$male <- gender$V1_Male
data$female <- gender$V1_Female
data$other<- gender$V1_Other

#data1$gender<-as.factor(data1$gender)

# data1$gender<- ifelse(data1$gender=='Male',1,0)


model1 = glm(data1$stroke~data1$gender+data1$age+data1$hypertension+data1$heart_disease+data1$ever_married+data1$Residence_type
            +data1$avg_glucose_level+data1$bmi+data1$children+data1$govtjob+data1$private+data1$neverworked
            +data1$formerlysm+data1$neversm+data1$smokes,family = 'binomial')
summary(model1)

with(model1,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))



anova(model1,test = 'Chisq')

## Prediction probabilities
p = predict(model1,type = 'response')
head(p)

p_test = predict(model1,type = 'response',newdata = testing)
head(p_test)

## Classification table
pred <- ifelse(p<0.5,0,1)
pred_test <- ifelse(p_test<0.5,0,1)

tab_train <- table(pred,actual = data1$stroke)
tab_train

# very bad model. only predicts 1 value for 1





model = step(glm(data1$stroke~data1$gender+data1$age+data1$hypertension+data1$heart_disease+data1$ever_married+data1$Residence_type
                 +data1$avg_glucose_level+data1$bmi+data1$children+data1$govtjob+data1$private+data1$neverworked
                 +data1$formerlysm+data1$neversm+data1$smokes,family = 'binomial'))
summary(model)

with(model,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))


anova(model,test = 'Chisq')

## Prediction probabilities
p = predict(model,type = 'response')
head(p)

## Classification table
pred <- ifelse(p<0.5,0,1)
pred_test <- ifelse(p_test<0.5,0,1)

tab_train <- table(pred,actual = data1$stroke)
tab_train

table(data1$stroke)


#######
boxplot(data$bmi)
data[which(data$bmi>80),]

str(data$stroke)



boxplot(data$avg_glucose_level)





#train-test splitting
smp_size<-floor(0.7*nrow(data))
set.seed(1024)
trainingdata <- sample(seq_len(nrow(data)),size=smp_size)
training<-data[trainingdata,]
testing<-data[-trainingdata,]

prop.table(table(training$stroke))
prop.table(table(testing$stroke))


stroke1 <- glm(stroke~ age + hypertension + heart_disease + ever_married +  Residence_type
               + avg_glucose_level + bmi+ children + govtjob + private + neverworked
               + formerlysm + neversm + smokes + female + male,data = training ,family = 'binomial')


summary(stroke1)


with(stroke1,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))


anova(stroke1,test = 'Chisq')





stroke_step <- step(stroke1)
with(stroke_step,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))

anova(stroke_step,test = 'Chisq')

summary(stroke_step)


p = predict(stroke_step,type = 'response')
head(p)

p_test = predict(stroke_step,type = 'response',newdata = testing)
head(p_test)

## Classification table
pred <- ifelse(p<0.5,0,1)
pred_test <- ifelse(p_test<0.5,0,1)

sum(pred)
sum(pred_test)

tab_train <- table(pred,actual = training$stroke)
tab_train

sumtab_train<- addmargins(tab_train,FUN=sum)
sumtab_train

tab_test <-table(pred_test,actual = testing$stroke)
tab_test
sumtab_test<- addmargins(tab_test,FUN=sum)
sumtab_test



TAP
TAN

TP

TAP <- sum(tab_test[,2])   #Total actual positives
TAN <- sum(tab_test[,1])   # Total actual negatives

TP <- sumtab_test[2,2]
TN <- sumtab_test[1,1]
FP <- sumtab_test[2,1]
FN <- sumtab_test[1,2]
TPR = TP/TAP    # Sensitivity or recall ## ability to correctly classify 
# positives
TPR

FPR = FP/TAN    
FPR

TNR = TN/TAN   # Specificity
TNR

#(3397/3577)


accuracy = (TP+TN)/(TAN+TAP)
accuracy
miss_classification_error = 1-accuracy
miss_classification_error

precision = TP/(TP+FP)
precision  # conditional probability of being positive when predicted positive

FPR <- FP/TAN
specificity <- TN/TAN
specificity





acc <- function(mod, pp, p,p_test) {
  out = c()
  
  ## Classification table
  pred <- ifelse(p<pp,0,1)
  pred_test <- ifelse(p_test<pp,0,1)
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
  out$cut_off = pp
  return(out)
}


a1 = acc(stroke_step,0.5,p,p_test)
a2 = acc(stroke_step,0.4,p,p_test)
a3 = acc(stroke_step,0.3,p,p_test)
a4 = acc(stroke_step,0.2,p,p_test)
a5 = acc(stroke_step,0.1,p,p_test)

a1$f_score
a2$f_score
a3$f_score
a4$f_score
a5$f_score

a1$specificity
a2$specificity
a3$specificity
a4$specificity
a5$specificity


a1$accuracy
a2$accuracy
a3$accuracy
a4$accuracy
a5$accuracy

a1$precision
a2$precision
a3$precision
a4$precision
a5$precision

a1$TPR
a2$TPR
a3$TPR
a4$TPR
a5$TPR

a1$FNR
a2$FNR
a3$FNR

acc(stroke_step,0.2,p,p_test)$f_score

acc(stroke_step,0.2,p,p_test)

#install.packages('ROCR')
library(ROCR)
ROCRpred = prediction(p,training$stroke)
ROCRperf = performance(ROCRpred,'tpr','fpr')
ROCRperf

plot(ROCRperf,colorize = T)
abline(a=0,b=1)

auc<- performance(ROCRpred,measure = 'auc')
auc <- auc@y.values[[1]]
auc



######### over-under sampling
#install.packages('ROSE')
library(ROSE)

over = ovun.sample(stroke~.,data = data,method = 'over',N = 6000,seed = 1024)
prop.table(table(over$data$stroke))

# store the old data in df1
df1 = data 

data = over$data








plot(data$age,data$bmi, col = c("Red","Blue","Green","Yellow","Purple")[data$work_type])


c("Red","Blue")[data$stroke]
