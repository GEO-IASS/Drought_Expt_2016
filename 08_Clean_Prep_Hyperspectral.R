#Function to format Hyperspectral files for formatting and analysis
#Need to average all 9 spectra together, or get median spectra (?)
#So here are the steps that need to happen:
#1) For each leaf on each date: create a file that plots all 9 spectra and then plots the average spectra
#This will be for quality control in order to determine whether there are any weird spectra that should be 
#removed, for example
#2)Once average spectra are all created and oragnized, then create a file with the (average) spectra for
#each date for that plant. SO each plant will have one file containing 8 or 9 spectra (which are themselves the 
#average of 9 spectra)

library(dplyr)
library(readr)

#Step 1 - create file that plots all 9 spectra together and then averages them----------------

getwd()
setwd("C:/Users/rsstudent//Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/")

#this command creates a list based on the pattern: used http://regexr.com/ to test expression before using
file_list <- list.files(pattern = '.*A18*......5-24.*')
datalist <- lapply(file_list, function(x){read.table(file=x)})

 
hlist <- list.files(pattern = '.*A18*......5-24.*')

#User Defined Function: Format Hyperspec: 

#Create Column with filename and then delete the first row of data frame
Format_hyperspec <- function(test){
        test$filename <- (test$reflectance[1:1])
        test = test[-1,]
        
        #Change factors to numeric
        test$wavelength <- as.numeric(levels(test$wavelength))[test$wavelength]
        test$reflectance <-as.numeric(levels(test$reflectance))[test$reflectance]
        
        test
}


df <- lapply(hlist, FUN=read.table, col.names=c("wavelength", "reflectance")) %>% 
      lapply(FUN=Format_hyperspec) %>%
        bind_cols

head(df)
str(df)
