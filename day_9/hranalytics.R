
getwd()
rm(list = ls())

install.packages('readr')
install.packages('ggplot2')
install.packages('mlbench')
install.packages('corrplot')
install.packages('Amelia')
install.packages('caret')
install.package('plotly')
install.packages('caTools')
install.packages('reshape2')
install.packages('dplyr')
hr = read.csv(file.choose("HR.csv"),T)
# THIS DATASET CONTAINS 10 VARIABLES 
str(hr)
sum(is.na(hr))

hr$Work_accident = as.factor(hr$Work_accident)
hr$promotion_last_5years = as.factor(hr$promotion_last_5years)

summary(hr)
library(dplyr)

#########SATISFACTION LEVEL
library('ggplot2')
ggplot(hr, aes(x=hr$satisfaction_level)) + geom_density(fill="violet") 
summary(hr$satisfaction_level)
######LAST EVALUATION VARIABLE
ggplot(hr, aes(x=hr$last_evaluation)) + geom_density(fill="violet") 
summary(hr$last_evaluation)
########NOOF PROJECTS###########
ggplot(hr, aes(x=hr$number_project)) + geom_bar(fill="blue") 
summary(hr$number_project)
###########AVERAGE  MONTHLY HOURS
ggplot(hr, aes(x=hr$average_montly_hours)) + geom_density(fill="green") 
summary(hr$number_project)
############TIME VARIABLE##########
ggplot(hr, aes(x=hr$time_spend_company)) + geom_bar(fill="blue") 
summary(hr$time_spend_company)
##############THREE VARIABLES IN ONE GRID -- Work_accident :left: promotion_last_5year
plot1<-ggplot(hr, aes(x=hr$Work_accident)) + geom_bar(fill="blue")
plot2<-ggplot(hr, aes(x=hr$left)) + geom_bar(fill="blue")
plot3<-ggplot(hr, aes(x=hr$promotion_last_5years)) + geom_bar(fill="blue")
gridExtra::grid.arrange(plot1,plot2,plot3,nrow=1)
#################SALES AND SALARY##############
plot4<-ggplot(hr, aes(x=hr$sales)) + geom_bar(fill="blue")
hr$salary2 <- factor(hr$salary, levels = c("low", "medium", "high"))
plot5<-ggplot(hr, aes(x=hr$salary2)) + geom_bar(fill="blue")
gridExtra::grid.arrange(plot4,plot5,nrow=1)

colnames(hr)

#Boxplots of all the Quant columns
par(mfrow=c(2,3))

boxplot(hr$satisfaction_level, main="satisfaction_level", col ="red")
boxplot(hr$last_evaluation, main="last_evaluation", col ="red")
boxplot(hr$number_project, main="number_project", col ="red")
boxplot(hr$average_montly_hours, main="average_montly_hours", col ="red")
boxplot(hr$time_spend_company, main="time_spend_company", col ="red")
boxplot(log(hr$time_spend_company), main="log_time_spend_company", col ="red")

############BIVARIATE ANALYSIS#############
######new variable#########
hours_projects_ratio <- round(hr$average_montly_hours/hr$number_project,2)
########scatter plot
#Reorder the dataset to isolate at the last position the categorical varibales
hr_corelation <- hr %>% select(satisfaction_level:hours_projects_ratio)
hr1 <- cor(hr[1,2,3,4,5,6,7,8,9])

########### Factor and integer column and create a matrix correlation
library(corrplot)
par(mfrow=c(1,1))

Quant_colnames <- c ("satisfaction_level", "last_evaluation", "number_project", "average_montly_hours", "time_spend_company")

plot(hr[,Quant_colnames])

cor(hr[,Quant_colnames])

corrplot(round(cor(hr[,Quant_colnames]),2), method = "number")

##############LAST EVALUATION----NO:OF PROJECTS

par(mfrow=c(2,2))

ggplot(data = hr, aes(x = factor(number_project), y = last_evaluation, fill =factor(number_project) ))+
geom_boxplot( colour = "#1F3552",
                alpha = 0.6)
##########LAST EVALUATION------AVG MONTHLY HOURS
ggplot(data = hr, aes(x = average_montly_hours, y = last_evaluation, fill =factor(number_project) ))+
geom_boxplot( colour = "#1F3552",
                alpha = 0.6)
############AVG MONTHLY HOURS -----NO:OF PROJECTS
ggplot(data = hr, aes(x = number_project, y = average_montly_hours, fill =factor(number_project) ))+
geom_boxplot( colour = "#1F3552",
                alpha = 0.6)
###########LEFT------------SATISFACTION
ggplot(data = hr, aes(x = left,y= satisfaction_level, fill =factor(number_project) ))+
geom_boxplot( colour = "#1F3552",
 alpha = 0.6)
ggplot(data = hr, aes(x = left, y = satisfaction_level, fill =factor(number_project) ))+
geom_jitter( colour = "#1F3552",
                                alpha = 0.6) 
############LEFT------WORK ACCIDENT########
ggplot(data = hr, aes(x = factor(left), y = Work_accident, fill =factor(number_project) ))+
  geom_jitter( colour = "#1F3552",
               alpha = 0.6)
############LEFT------TIME SPENT########
ggplot(data = hr, aes(x = factor(left), y = time_spend_company, fill =factor(number_project) ))+
geom_jitter( colour = "#1F3552",
               alpha = 0.6)
#############LEFT----SALES
##heat map 
hr$performance <- rnorm(nrow(hr))
ggplot(data = hr, aes(x = sales, y = left)) +
geom_tile(aes(fill = performance))


##########LEFT----------SALARY
ggplot(data = hr, aes(x= salary, y = left,fill =factor(number_project) )) +
geom_boxplot(colour = "#1F3552",
               alpha = 0.6)
############Multivariate ANALYSIS
###########NUMBER OF PROJECTS----LAST EVALUATION-----LEFT
ggplot(data = hr, aes(x = factor(number_project), y = last_evaluation, fill =factor(number_project) ))+
geom_boxplot( colour = "#1F3552",
alpha = 0.6)+   facet_wrap(~left, ncol = 2)
############NUMBER OF PROJECTS-----AVERAGE MONTHLY----LEFT
ggplot(data = hr, aes(x = factor(number_project), y = average_montly_hours, fill = factor(number_project)))+
geom_boxplot(colour = "#1F3552",
alpha = 0.6)+  facet_wrap(~left, ncol = 2)
#########SATISFACTION LEVEL-----LAST EVALUATION-------
ggplot(data = hr, aes(x = satisfaction_level, y = last_evaluation, color = factor(left)))+
geom_jitter(alpha = 0.5)
############SATISACTION LEVEL-----LAST EVALUATION------NUMBER OF PROJECT
ggplot(data = hr, aes(x = satisfaction_level, y = last_evaluation, color = factor(left)))+
geom_jitter(alpha = 0.5)+  facet_wrap(~number_project, ncol= 2, nrow = 3)
#########SATISFACTION LEVEL-----LAST EVALUATION-------SALARY
ggplot(data = hr, aes(x = satisfaction_level, y = last_evaluation, color = factor(left)))+
geom_jitter(alpha = 0.5)+  facet_wrap(~salary2, ncol= 2, nrow = 2)
################SATISFACTION LEVEL------LAST EVALUATION----SALARY
ggplot(data = hr, aes(x = satisfaction_level, y = last_evaluation, color = factor(left)))+
  geom_jitter(alpha = 0.5)+  facet_wrap(~salary2, ncol= 2, nrow = 2)
#############FINAL PLOT
#############AVERAGE MONTHLY HOURS---------NUMBER OF PROJECTS-----PLOT1
ggplot(data = hr, aes(x = factor(number_project), y = average_montly_hours, fill = factor(number_project)))+
  geom_boxplot(colour = "#1F3552", alpha = 0.6)+ facet_wrap(~left, ncol = 2)
###############NUMBER OF HOURS---SATISFACTION LEVEL---LAST EVALUATION LEVEL
ggplot(data = hr, aes(x = satisfaction_level, y = last_evaluation, color = factor(left)))+
  geom_jitter(alpha = 0.5)+  facet_wrap(~number_project, ncol= 2, nrow = 3)


