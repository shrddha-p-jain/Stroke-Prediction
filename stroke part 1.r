data <- read.csv("C:\\Users\\ABC\\Downloads\\stroke\\healthcare-dataset-stroke-data.csv")
str(data)

#drop id col
data = data[-1]

#preprocessing bmi
sum(data$bmi=='N/A')
data$bmi[data$bmi=='N/A'] <- NA
data$bmi = as.numeric(data$bmi)

# chang char to factor
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
data$hypertension = as.factor(data$hypertension)
data$heart_disease = as.factor(data$heart_disease)
data$stroke = as.factor(data$stroke)
str(data)

#checking for na's
lapply(data,function(x) { length(which(is.na(x)))})
lapply(data,function(x) {levels(x)})


# checking for dimension of data
dim(data)
#  chaecking for proportion of patients that had a stroke attack, when bmi is NA
sum(data$stroke[is.na(data$bmi)]==1)
sum(data$stroke[is.na(data$bmi)]==1)/length(data$stroke[is.na(data$bmi)])

#proportion of patients with that had a stroke attack in whole dataset
tabl = table(data$stroke)
pt = prop.table(tabl)

str(pt)
pie(pt,labels = round(pt*100,2)/100, col = c("aquamarine","slateblue1"))
legend(x='topright',legend = c("No Stroke","Stroke"), col = c("aquamarine","slateblue1"),pch=19)




# Imputation with MICE
#install.packages('mice')
library(mice)
md.pattern(data)
imp <- mice(data)
summary(imp)
imp$imp$bmi
data.mice = complete(imp,4)

str(data.mice)

data = data.mice






# EDA
lapply(data,function(x) {if(is.factor(x)) {prop.table(table(x,data$stroke,dnn = c('var','Stroke')),margin = 1)}})


tabl <- table(data$gender)
tabl
prop.table(tabl)

tabl <- table(data$gender,data$stroke)
tabl
prop.table(tabl,margin=1)
prop.table(tabl,margin=2)
barplot(t(tabl))
barplot(t(prop.table(tabl,margin=1)))

chisq.test(tabl[1:2,],correct = F)
# prop.test(tabl[1:2,])


tabl <- table(data$hypertension)
tabl
prop.table(tabl)

tabl <- table(data$hypertension,data$stroke,dnn = c('hypertension','stroke'))
tabl
prop.table(tabl,margin=1)
prop.table(tabl,margin=2)
barplot(t(tabl),xlab = 'hypertension', ylab = 'patients')
barplot(t(prop.table(tabl,margin=1)),xlab = 'hypertension', ylab = 'patients proprtion')
chisq.test(tabl,correct = F)

# prop.test(tabl)

tabl <- table(data$heart_disease)
tabl
prop.table(tabl)

tabl <- table(data$heart_disease,data$stroke,dnn = c('heart disease','stroke'))
tabl
prop.table(tabl,margin=1)
prop.table(tabl,margin=2)
barplot(t(tabl),xlab = 'heart disease', ylab = 'patients')
barplot(t(prop.table(tabl,margin=1)),xlab = 'heart disease', ylab = 'patients proprtion')
chisq.test(tabl,correct = F)


tabl <- table(data$ever_married)
tabl
prop.table(tabl)

tabl <- table(data$ever_married,data$stroke)
tabl
prop.table(tabl,margin=1)
prop.table(tabl,margin=2)
barplot(t(tabl),xlab = 'ever married', ylab = 'patients')
barplot(t(prop.table(tabl,margin=1)),xlab = 'ever married', ylab = 'patients proprtion')
chisq.test(tabl,correct = F)


#plot(data$age,as.numeric(data$ever_married), col = c('Red','Black')[data$stroke])
#plot(data$age[data$stroke==1],as.numeric(data$ever_married[data$stroke==1]))
#data$age[data$age<10]

tabl <- table(data$Residence_type)
tabl
prop.table(tabl)

tabl <- table(data$Residence_type,data$stroke)
tabl
prop.table(tabl,margin=1)
prop.table(tabl,margin=2)
barplot(t(tabl),xlab = 'residence type', ylab = 'people')
barplot(t(prop.table(tabl,margin=1)),xlab = 'residence type', ylab = 'proportion')
chisq.test(tabl,correct = F)


tabl <- table(data$work_type)
tabl
prop.table(tabl)
pie(tabl)

tabl <- table(data$work_type,data$stroke)
tabl
prop.table(tabl,margin=1)
prop.table(tabl,margin=2)
barplot(t(tabl),xlab = 'work type', ylab = 'patients')
barplot(t(prop.table(tabl,margin=1)),xlab = 'work type', ylab = 'patients proprtion')

tabl <- table(data$smoking_status)
tabl
prop.table(tabl)
pie(tabl)


tabl <- table(data$smoking_status,data$stroke)
tabl
prop.table(tabl,margin=1)
prop.table(tabl,margin=2)
barplot(t(tabl),xlab = 'smoking status', ylab = 'patients')
barplot(t(prop.table(tabl,margin=1)),xlab = 'smoking status', ylab = 'patients proprtion')
chisq.test(tabl,correct = F)



plot(data$age,data$stroke)
library(ggplot2)
ggplot()+geom_bar(data=data,
                    aes(x=factor(data$age),fill=factor(data$stroke)),
                    position= "fill")+ scale_x_discrete("
                    age")+scale_y_continuous("Percent")+
  guides(fill=guide_legend(title ="data"))+scale_fill_manual(values=c("blue","red"))

ggplot()+geom_bar(data=data,
                  aes(x=factor(data$age),fill=factor(data$stroke)),
                  position= "stack")+ scale_x_discrete("
                                                       age")+scale_y_continuous("Percent")+
  guides(fill=guide_legend(title ="data"))+scale_fill_manual(values=c("blue","red"))



plot(data$avg_glucose_level,data$stroke)
ggplot()+geom_bar(data=data,
                  aes(x=factor(data$avg_glucose_level),fill=factor(data$stroke)),
                  position= "fill")+ scale_x_discrete("
                                                      glucose level")+scale_y_continuous("Percent")+
  guides(fill=guide_legend(title ="data"))+scale_fill_manual(values=c("blue","red"))

ggplot()+geom_bar(data=data,
                  aes(x=factor(data$avg_glucose_level),fill=factor(data$stroke)),
                  position= "stack")+ scale_x_discrete("
                                                      glucose level")+scale_y_continuous("Percent")+
  guides(fill=guide_legend(title ="data"))+scale_fill_manual(values=c("blue","red"))


plot(data$bmi,data$stroke)
ggplot()+geom_bar(data=data,
                  aes(x=factor(data$bmi),fill=factor(data$stroke)),
                  position= "fill")+ scale_x_discrete("
                                                      bmi")+scale_y_continuous("Percent")+
  guides(fill=guide_legend(title ="data"))+scale_fill_manual(values=c("blue","red"))

ggplot()+geom_bar(data=data,
                  aes(x=factor(data$bmi),fill=factor(data$stroke)),
                  position= "stack")+ scale_x_discrete("
                                                       bmi")+scale_y_continuous("Percent")+
  guides(fill=guide_legend(title ="data"))+scale_fill_manual(values=c("blue","red"))



range(data$avg_glucose_level)
breaks <- c(50,100,150,200,250,300)
group_tags <- cut(data$avg_glucose_level,
                  breaks=breaks,
                  include.lowest=TRUE,
                  right=FALSE)
summary(group_tags)
data$glucose <- factor(group_tags,ordered = TRUE)

tabl <- table(data$glucose)
tabl
prop.table(tabl)

tabl <- table(data$glucose,data$stroke)
tabl
prop.table(tabl,margin=1)
prop.table(tabl,margin=2)
barplot(t(tabl))
barplot(t(prop.table(tabl,margin=1)),xlab = "Glucose",ylab = 'Proportion')


range(data$age)

breaks <- c(0,10,20,30,40,50,60,70,80,90,100)
group_tags <- cut(data$age,
                  breaks=breaks,
                  include.lowest=TRUE,
                  right=FALSE)
summary(group_tags)
data$agebins <- factor(group_tags,ordered = TRUE)

tabl <- table(data$agebins)
tabl
prop.table(tabl)

tabl <- table(data$agebins,data$stroke)

tabl
prop.table(tabl,margin=1)
prop.table(tabl,margin=2)
barplot(t(tabl))
barplot(t(prop.table(tabl,margin=1)),xlab = "Age",ylab = 'Proportion')

#prop.test(tabl)


range(data$bmi)
breaks <- c(10,20,30,40,50,60,70,80,90,100)
group_tags <- cut(data$bmi,
                  breaks=breaks,
                  include.lowest=TRUE,
                  right=FALSE)
summary(group_tags)
data$bmibins <- factor(group_tags,ordered = TRUE)

tabl <- table(data$bmibins)
tabl
prop.table(tabl)

tabl <- table(data$bmibins,data$stroke)
tabl
prop.table(tabl,margin=1)
prop.table(tabl,margin=2)
barplot(t(tabl),xlab = "bmi",ylab = 'Patients')
barplot(t(prop.table(tabl,margin=1)),xlab = "bmi",ylab = 'Proportion') 

''' 
cor(data.mice$age,data.mice$bmi)
cor(data.mice$avg_glucose_level,data.mice$bmi)
cor(data.mice$age,data.mice$avg_glucose_level)

cor.test(data.mice$age,data.mice$bmi)
cor.test(data.mice$avg_glucose_level,data.mice$bmi)
cor.test(data.mice$age,data.mice$avg_glucose_level)

cor.test(data$age,as.numeric(data$work_type),method = 'kendall')

cor.test(data$age,as.numeric(data$Residence_type),method = 'kendall')
pairs(data)


lapply(data,function(x) {cor.test(as.numeric(x),as.numeric(data$gender),method = 'kendall')})
'''       

data1<-data[data$gender != 'Other',]
data1
table(data1$gender,data1$ever_married)[-3,]
chisq.test(table(data1$gender,data1$ever_married)[-3,])

chisq.test(table(data1$gender,data1$heart_disease)[-3,])

dim(data1)

#install.packages('car')
library(car)

data$worktype_binary <- ifelse(data$work_type=="children"|data$work_type=="Never_worked",'Non-working','Working')
data$worktype_binary<- as.factor(data$worktype_binary)
levels(data$worktype_binary)


model = glm(worktype_binary~age+ever_married+bmi,data=data,family = 'binomial')
summary(model)

plot(data$age,as.numeric(data$worktype_binary))

levels(data$work_type)



model = glm(worktype_binary~age,data=data,family = 'binomial')


vif(model)

model = glm(stroke~.-bmibins, data=data, family = 'binomial')

summary(model)

with(model,pchisq(null.deviance-deviance,df.null-df.residual,lower.tail = F))



tabl <- table(data$agebins,data$worktype_binary)

tabl
prop.table(tabl,margin=1)
prop.table(tabl,margin=2)
barplot(t(tabl))
barplot(t(prop.table(tabl,margin=1)),xlab = "Age",ylab = 'Proportion')







md = lm(bmi~age+work_type+Residence_type+heart_disease+hypertension+avg_glucose_level,data=data)
summary(md)
vif(md)

md = glm(hypertension~age+work_type+Residence_type+bmi+heart_disease+avg_glucose_level,data=data,family = 'binomial')
summary(md)
vif(md)


md = lm(age~bmi+work_type+Residence_type+heart_disease+hypertension+avg_glucose_level+ever_married,data=data)
summary(md)
vif(md)


names(data)



#install.packages('mltools')
library(mltools)
library(data.table)
gen <- one_hot(as.data.table(data$gender))
head(gen)
dim(gen)

#not needed
hypt <- one_hot(as.data.table(data$hypertension))
head(hypt)
dim(hypt)
colnames(hypt) = c('No','Yes')

workt <- one_hot(as.data.table(data$work_type))
head(workt)
dim(workt)


smoke <- one_hot(as.data.table(data$smoking_status))
head(smoke)
dim(smoke)    

names(data)
