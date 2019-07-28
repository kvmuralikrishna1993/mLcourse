## Quality Measures
## Supprot <- This says how popular an itemset is, 
##            as measured by the proportion of transactions in which an itemset appears.

## Confidence <- This says how likely item Y is purchased when item X is purchased, expressed as {X -> Y}. 
##              This is measured by the proportion of transactions with item X, in which item Y also appears.


## Lift <- This says how likely item Y is purchased when item X is purchased, 
##         while controlling for how popular item Y is.
##         A lift value greater than 1 means that item Y is likely to be bought if item X is bought, 
##         while a value less than 1 means that item Y is unlikely to be bought if item X is bought.

##load the required package
library(arules)

setwd("D:\\Personal\\ITD\\DataScienceCourse\\13.Unsupervised Techniques - Applications (3 Hrs)\\ClassDemos-RFiles\\")

##Read Data
myData <- read.csv("Cosmetics.csv", colClasses = "factor", header = T)

##Remove transaction number column
myData <- myData[, -1]

##Summary of items
summary(myData)

##find association rules
rules <- apriori(myData)

##summary of rules
summary(rules)

##reduce to smaller number of rules
rules <- apriori(myData, parameter = list(minlen = 2, maxlen = 3, support = .7))

##inspecting rules
inspect(rules)

##finding interesting rules
rules <- apriori(myData, parameter = list(minlen = 2, maxlen = 3, conf = .7),
                 appearance = list(rhs = c("Foundation=1"), default = "lhs"))
inspect(rules)


##visulaizing rules
library(arulesViz)
plot(rules)
plot(rules, method = "grouped")
plot(rules, method = "graph", control = list(type="items"))

##finding interesting rule - contd
rules <- apriori(myData, parameter = list(minlen = 2, maxlen = 5, supp = .1, conf = .5),
                 appearance = list(rhs = c("Foundation=1"), lhs = c("Bag=1", "Blush=1", "Nail.Polish=1",
                                                                    "Brushes=1", "Concealer=1",
                                                                    "Eyebrow.Pencils=1", "Bronzer=1",
                                                                    "Lip.liner=1", "Mascara=1",
                                                                    "Eye.shadow=1", "Lip.Gloss=1",
                                                                    "Lipstick=1", "Eyeliner=1"), 
                                   default = "none"))
quality(rules) <- round(quality(rules), digits = 3)
inspect(rules)
