library(ggpubr)
head(mtcars)
str(mtcars)
names(mtcars)
tail(mtcars)
summary(mycars)
mode_mpg <- names(sort(-table(mtcars$mpg)))[1]
mode_cyl <- names(sort(-table(mtcars$cyl)))[1]
mode_disp <- names(sort(-table(mtcars$disp)))[1]
mode_hp <- names(sort(-table(mtcars$hp)))[1]
mode_drat <- names(sort(-table(mtcars$drat)))[1]
mode_wt <- names(sort(-table(mtcars$wt)))[1]
mode_qsec <- names(sort(-table(mtcars$qsec)))[1]
mode_vs <- names(sort(-table(mtcars$vs)))[1]
mode_am <- names(sort(-table(mtcars$am)))[1]
mode_gear <- names(sort(-table(mtcars$gear)))[1]
mode_carb <- names(sort(-table(mtcars$carb)))[1]
###########Paste
paste("The mode of the miles per gallon data is", mode_mpg)
paste("The mode of the number of cylinders data is", mode_cyl)
paste("The mode of the displacement data is", mode_disp)
paste("The mode of the horsepower data is", mode_hp)
paste("The mode of the rear axle ratio data is", mode_drat)
paste("The mode of the weight (1000 lbs) data is", mode_wt)
paste("The mode of the quarter mile time data is", mode_qsec)
paste("The mode of the V/S data is", mode_vs)
paste("The mode of the transmission data is", mode_am)
paste("The mode of the transmission data is", mode_am)
paste("The mode of the number of forward gears data is", mode_gear)
paste("The mode of the number of carburetors data is", mode_carb)
library(ggplot2)
ggplot(mtcars, aes(mpg)) +
  geom_histogram(binwidth = 4) + xlab('Miles per Gallon') + ylab('Number of Cars') + 
  ggtitle('Distribution of Cars by Mileage')
ggplot(mtcars, aes(cyl)) +
  geom_histogram(binwidth=1) + xlab('Cylinders') + ylab('Number of Cars') +
  ggtitle('Distribution of Cars by Cylinders')
ggplot(mtcars, aes(hp)) +
  geom_histogram(binwidth=20) + xlab('horsepower') + ylab('Number of Cars') +
  ggtitle('Distribution of Cars by Horsepower')
cor(mtcars$mpg, mtcars$hp)
library(corrplot)


cor(mtcars$mpg, mtcars$hp)
ggplot(mtcars, aes(hp, mpg)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Miles per Gallon") +
  xlab("No. of Horsepower") +
  ggtitle("Impact of Number of Horsepower on MPG")

cor(mtcars$mpg, mtcars$cyl)

qplot(cyl, mpg, data = mtcars, colour = cyl, geom = "point",     
      ylab = "Miles per Gallon", xlab = "No. of Cylinders",
      main = "Impact of Number of Cylinders on MPG")     

ggplot(mtcars, aes(cyl, mpg)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Miles per Gallon") + xlab("No. of Cylinders") +
  ggtitle("Impact of Number of Cylinders on MPG")

cor<-cor(mtcars)
corrplot(cor, method="number")
