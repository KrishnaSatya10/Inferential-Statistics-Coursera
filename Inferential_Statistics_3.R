library(statsr)
library(dplyr)
library(ggplot2)
library(tidyr)

#In 2004, the state of North Carolina released a large data set containing information on births recorded in this state. This data set is useful to researchers studying the relation between habits and practices of expectant mothers and the birth of their children.
#We will work with a random sample of observations from this data set.
nc
str(nc)

#Exploratory Data Analysis
summary(nc$gained) 
boxplot(weight~habit, data = nc)

inference(y= weight, x = habit,data = nc, statistic = "mean", type = 'HT',null = 0,alternative = 'twosided',method = 'theoretical')
inference(y= weight, x = habit,data = nc, statistic = "mean", type = 'ci',null = 0,method = 'theoretical',conf_level = 0.99) #The value conf_level indicated the confidence interval that it can have. It is given between 0 to 1. 
#default is 0.95 corresponding to 95 percent confdence interval

#Inference and 99 percent CI for average length of pregnancies. THis is a continous thing. 
inference(y= weeks,data = nc, statistic = "mean", type = 'ci',method = 'theoretical',conf_level = 0.99) 

df_spread <- spread(data = nc,key = 'mature', value = 'mage',drop = TRUE)
summary(df_spread$`mature mom`)
