age.c = as.cha

age = one_hot(as.data.table(as.factor(age.c)))
head(age.enc)
dim(age.enc)

names(data)

age.c = as.character(data$agebins)


data$a0_10 = ifelse(data$age<=10,1,0)
data$a10_20 = ifelse(data$age>10&data$age<=20,1,0) 
data$a20_30 = ifelse(data$age>20&data$age<=30,1,0)
data$a30_40 = ifelse(data$age>30&data$age<=40,1,0)
data$a40_50 = ifelse(data$age>40&data$age<=50,1,0)
data$a50_60 = ifelse(data$age>50&data$age<=60,1,0)
data$a60_70 = ifelse(data$age>60&data$age<=70,1,0)
data$a70_80 = ifelse(data$age>70&data$age<=80,1,0)
data$a80_90 = ifelse(data$age>80&data$age<=90,1,0)


data$b0_10 = ifelse(data$bmi<=10,1,0)
data$b10_20 = ifelse(data$bmi>10&data$bmi<=20,1,0) 
data$b20_30 = ifelse(data$bmi>20&data$bmi<=30,1,0)

data$b30_40 = ifelse(data$bmi>30&data$bmi<=40,1,0)
data$b40_50 = ifelse(data$bmi>40&data$bmi<=50,1,0)
data$b50_60 = ifelse(data$bmi>50&data$bmi<=60,1,0)
data$b60_70 = ifelse(data$bmi>60&data$bmi<=70,1,0)
data$b70_80 = ifelse(data$bmi>70&data$bmi<=80,1,0)
data$b80_90 = ifelse(data$bmi>80&data$bmi<=90,1,0)
data$b90_100 = ifelse(data$bmi>90&data$bmi<=100,1,0)

range(data$avg_glucose_level)

data$g50_100 = ifelse(data$avg_glucose_level>50&data$bmi<=100,1,0) 
data$g100_150 = ifelse(data$avg_glucose_level>100&data$bmi<=150,1,0)
data$g150_200 = ifelse(data$avg_glucose_level>150&data$bmi<=200,1,0)
data$g200_250 = ifelse(data$avg_glucose_level>200&data$bmi<=250,1,0)
data$g250_300 = ifelse(data$avg_glucose_level>250&data$bmi<=300,1,0)


names(data)

data_complete_over = data

data = data[,c(-1,-2,-6,-8,-9,-10,-12,-13,-14,-27,-28,-29)]

names(data)

smp_size<-floor(0.7*nrow(data))
set.seed(1024)
trainingdata <- sample(seq_len(nrow(data)),size=smp_size)
training<-data[trainingdata,]
testing<-data[-trainingdata,]

prop.table(table(training$stroke))
prop.table(table(testing$stroke))

stroke_step= step(glm(stroke~.,data=training,family = 'binomial'))
