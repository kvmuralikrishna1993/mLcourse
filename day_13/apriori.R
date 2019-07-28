# Data Preprocessing
# install.packages('arules')
library(arules)

getwd()
setwd("D:\\Personal\\ITD\\DataScienceCourse\\13.Unsupervised Techniques - Applications (3 Hrs)\\ClassDemos-RFiles\\")

dataset = read.csv('Market_Basket_Optimisation.csv', header = FALSE)
dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)



itemFrequencyPlot(dataset, topN = 15)

# Training Apriori on the dataset
rules = apriori(data = dataset, parameter = list(support = 0.004, confidence = 0.2))

# Visualising the results
inspect(sort(rules, by = 'lift')[1:15])
