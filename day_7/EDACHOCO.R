rm(list = ls())
library(DataExplorer)
choco <- read.csv(file.choose("flavors_of_cacao.csv"),T)
### Some Format changes in the input data. 
choco$Cocoa.Percent <- as.numeric(gsub('%','',choco$Cocoa.Percent))
choco$Review.Date <- as.character(choco$Review.Date)
### Variables 

#The very first thing that you'd want to do in your EDA is checking the dimension of the input dataset and the time of variables. 
plot_str(choco)
#With that, we can see we've got some Continuous variables and some Categorical variables. 

### Man's search for Missing Values
plot_missing(choco)
#And we are fortunate that there's no missing value in this dataset. 

### Histogram of Continuous Variables
plot_histogram(choco)
### Perhaps, you are a fan of Density plot
plot_density(choco)
### Colorful Correlation Plot
plot_correlation(choco, type = 'continuous','Review.Date')
### Time for some Categorical Variables - Barplots!
plot_bar(choco) 
#And finally, if you have got only a couple of minutes (just like in the maggi noodles ad, 2 mins!) just keep it simple to use `create_report()`
create_report(choco) #comment this if you're not rendering this entire rmarkdown