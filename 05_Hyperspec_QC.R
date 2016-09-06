#Author: Mallory Barnes
#Date: 09/5/2016 
#Purpose: Quality check Hyperspectral Files for any obvious outliers 
#Input: Hyperspectral  Reflectances in ASCII format
#Output: Quality Checked Hyperspectral Reflectances 

library(ggplot2)
library(hyperSpec)

#Set working directory to location fo hyperspectral ASD Files
getwd()
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")

#Read in file, used "col.names" argument to rename columns properly. Will need to figure out
#how to do this with a list of ASCII files
test <- read.table("ASCII_Reflectance/b2popmlb_A1_Leaf_5-20-201600000.asd.txt", col.names=c("wavelength", "reflectance"))

#Create Column with filename and then delete the first row of data frame
test$filename <- (test$reflectance[1:1])
test = test[-1,]

#Change factors to numeric
as.numeric(levels(test$wavelength))[test$wavelength]
as.numeric(levels(test$reflectance))[test$reflectance]



str(test)
head(test)
ggplot(test, aes(x = wavelength, y = reflectance, group=1)) + geom_smooth(method=loess)
str(barbiturates)
head(barbiturates)
