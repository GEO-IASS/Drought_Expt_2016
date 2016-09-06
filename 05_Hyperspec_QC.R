#Author: Mallory Barnes
#Date: 09/5/2016 
#Purpose: Quality check Hyperspectral Files for any obvious outliers 
#Input: Hyperspectral  Reflectances in ASCII format
#Output: Quality Checked Hyperspectral Reflectances 

library(ggplot2)
library(hyperSpec)

getwd()
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")
test <- read.table("ASCII_Reflectance/b2popmlb_A1_Leaf_5-20-201600000.asd.txt", col.names=c("wavelength", "reflectance"))
test$filename <- (test$reflectance[1:1])
test = test[-1,]
as.numeric(levels(test$wavelength))[test$wavelength]
as.numeric(levels(test$reflectance))[test$reflectance]
str(test)
head(test)
ggplot(test, aes(x = wavelength, y = reflectance, group=1)) + geom_smooth(method=loess)
str(barbiturates)
head(barbiturates)
