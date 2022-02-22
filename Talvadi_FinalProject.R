library(ggplot2)

library(readr)

system("ls ../input")

install.packages("ggcorrplot")
install.packages("ROCR")

library(ggcorrplot)
library(ROCR)

diabData = read.csv('/Users/itsmt/Desktop/diabetes.csv', header=TRUE)

print(diabData)

head(diabData)
tail(diabData)
str(diabData)

summary(diabData)

data_zero <- diabData [apply(diabData, 1, function(row) all (row !=0)), ]

view(data_zero)


diabData[apply(diabData==0, 1, all),]

diabData

summary(data_zero)

diabData$Age_Cat <- ifelse(diabData$Age < 21, "<21", 
                     ifelse((diabData$Age>=21) & (diabData$Age<=25), "21-25", 
                            ifelse((diabData$Age>25) & (diabData$Age<=30), "25-30",
                                   ifelse((diabData$Age>30) & (diabData$Age<=35), "30-35",
                                          ifelse((diabData$Age>35) & (diabData$Age<=40), "35-40",
                                                 ifelse((diabData$Age>40) & (diabData$Age<=50), "40-50",
                                                        ifelse((diabData$Age>50) & (diabData$Age<=60), "50-60",">60")))))))


diabData$Age_Cat <- factor(diabData$Age_Cat, levels = c('<21','21-25','25-30','30-35','35-40','40-50','50-60','>60'))
table(diabData$Age_Cat)

library(ggplot2)
ggplot(aes(x = Age), data=diabData) +
  xlab("Age") +
  ylab("Total number of people") +
  geom_histogram(binwidth=5, color='blue', fill = "grey") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5))


preg = table(diabData$Pregnancies)
barplot(height = preg, main = "Distribution of Pregnancies", col= "#0571B0", xlab = "Pregnancies", ylab = "Count")
library(ggplot2)
ggplot(aes(x = preg), data=diabData) +
  xlab("Pregnancies") +
  ylab("Count") +
  geom_histogram(binwidth=5, color='blue', fill = "#0571B0") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5))

gluc = table(diabData$Glucose)
barplot(height = gluc, main = "Distribution of Glucose", col= "#0571B0", xlab = "Glucose", ylab = "Count")
library(ggplot2)
ggplot(aes(x = gluc), data=diabData) +
  xlab("Glucose") +
  ylab("Count") +
  geom_histogram(binwidth=5, color='blue', fill = "#0571B0") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5))

bp = table(diabData$BloodPressure)
barplot(height = bp, main = "Distribution of Blood Pressure", col= "#0571B0", xlab = "Blood Pressure", ylab = "Count")
library(ggplot2)
ggplot(aes(x = bp), data=diabData) +
  xlab("Glucose") +
  ylab("Count") +
  geom_histogram(binwidth=5, color='blue', fill = "#0571B0") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5))

sk = table(diabData$SkinThickness)
barplot(height = sk, main = "Distribution of Skin Thickness", col= "#0571B0", xlab = "Skin Thickness", ylab = "Count")
library(ggplot2)
ggplot(aes(x = sk), data=diabData) +
  xlab("Skin Thickness") +
  ylab("Count") +
  geom_histogram(binwidth=5, color='blue', fill = "#0571B0") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5))

insl = table(diabData$Insulin)
barplot(height = insl, main = "Distribution of Insulin", col= "#0571B0", xlab = "Insulin", ylab = "Count")
library(ggplot2)
ggplot(aes(x = insl), data=diabData) +
  xlab("insl") +
  ylab("Count") +
  geom_histogram(binwidth=5, color='blue', fill = "#0571B0") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5))


bmi = table(diabData$BMI)
barplot(height = bmi, main = "Distribution of BMI", col= "#0571B0", xlab = "Body Mass Index", ylab = "Count")
library(ggplot2)
ggplot(aes(x = bmi), data=diabData) +
  xlab("Body Mass Index") +
  ylab("Count") +
  geom_histogram(binwidth=5, color='blue', fill = "#0571B0") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5))

dpf = table(diabData$DiabetesPedigreeFunction)
barplot(height = dpf, main = "Distribution of dpf", col= "#0571B0", xlab = "Diabetes Pedigree Function", ylab = "Count")
library(ggplot2)
ggplot(aes(x = dpf), data=diabData) +
  xlab("Diabetes Pedigree Function") +
  ylab("Count") +
  geom_histogram(binwidth=5, color='blue', fill = "#0571B0") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5))

age = table(diabData$Age)
barplot(height = dpf, main = "Distribution of Age", col= "#0571B0", xlab = "Age", ylab = "Count")
library(ggplot2)
ggplot(aes(x = age), data=diabData) +
  xlab("Age") +
  ylab("Count") +
  geom_histogram(binwidth=5, color='blue', fill = "#0571B0") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5))


library(ggplot2)

ggplot(aes(x=Age_Cat, y = BMI), data = diabData) +
geom_boxplot(outlier.colour="blue", fill="red", outlier.shape=16,
               outlier.size=2, notch=FALSE) +
               xlab('Age Category')+
               ylab("Body Mass Index")+
               coord_cartesian(ylim = c(0,80))

by(diabData$BMI, diabData$Age_Cat, summary)

db_cor <- round(cor(diabData[1:8]),1)
db_cor

#Checking which age group is having how much contribution in the dataset
db_cor %>%
  group_by(age)%>%
  count()%>%
  filter(n>10)%>%
  ggplot()+
  geom_col(aes(age,n), fill= 'brown')+
  ggtitle(" AGE FREQUENCIES")

install.packages("corrplot")
library(corrplot)
library(ggcorrplot)


ggcorrplot(db_cor)

corrplot(db_cor, method="square", type="lower",)
