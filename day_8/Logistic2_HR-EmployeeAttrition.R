# Choose the Data set
HRDt <- read.csv(file.choose())

library(dplyr)
library(ggplot2)

#install.packages("ggthemes")
library(ggthemes)

#Data structure
str(HRDt)

# dimension of the data
dim(HRDt)

numeric_HRdata <- HRDt[,c(1,4,6,7,10,11,13,14,15,17,19,20,21,24,25,26,28:35)]
numeric_HRdata


# convert the Attrition innto numerical
numeric_Attrition  <-as.numeric(HRDt$Attrition)
numeric_Attrition

# add the column to the numerics dataset
numeric_HRdata = cbind(numeric_HRdata, numeric_Attrition)  

str(numeric_HRdata)

#install.packages("corrplot")
library(corrplot)
corr <- cor(numeric_HRdata)

corrplot(corr, method="shade")

l <- ggplot(HRDt, aes(OverTime,fill = Attrition))
l + geom_histogram(stat="count")
## who worked over time has more Attrition


# MaritalStatus vs Attiriton
l <- ggplot(HRDt, aes(MaritalStatus,fill = Attrition))
l + geom_histogram(stat="count")

# singles attrition is more

#JobRole vs Attrition
l <- ggplot(HRDt, aes(JobRole,fill = Attrition))
l + geom_histogram(stat="count")

tapply(as.numeric(HRDt$Attrition) - 1 ,HRDt$JobRole,mean)

# more attrition with Sales representative, HR and technicians

###Gender vs Attrition
l <- ggplot(HRDt, aes(Gender,fill = Attrition))
l + geom_histogram(stat="count")

tapply(as.numeric(HRDt$Attrition) - 1 ,HRDt$Gender,mean)

colnames(HRDt)

#colnames(HRDt)[which(names(HRDt) == "ï..Age")] <- "Age"

### MonthlyIncome vs. Age, by  color = Attrition
ggplot(HRDt, aes(MonthlyIncome, Age, color = Attrition)) + 
  geom_jitter() +
  ggtitle("MonthlyIncome vs. Age, by  color = Attrition ") 




HRDtnew = HRDt[,-c(6,9,22)]
str(HRDtnew)

model <- glm(Attrition ~ ., data = HRDtnew, family='binomial') 

summary(model)

predicted_glm <- predict(model, HRDtnew, type='response')
predicted_glm <- ifelse(predicted_glm > 0.5,1,0)

table(HRDtnew$Attrition, predicted_glm)

confusion<-table(HRDtnew$Attrition, predicted_glm)
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
