#Function to format Hyperspectral files for formatting and analysis
#Need to average all 9 spectra together, or get median spectra (?)
#So here are the steps that need to happen:
#1) For each leaf on each date: create a file that plots all 9 spectra and then plots the average spectra
#This will be for quality control in order to determine whether there are any weird spectra that should be 
#removed, for example
#2)Once average spectra are all created and oragnized, then create a file with the (average) spectra for
#each date for that plant. SO each plant will have one file containing 8 or 9 spectra (which are themselves the 
#average of 9 spectra)

library(plyr)
library(readr)

#Step 1 - create file that plots all 9 spectra together and then averages them----------------

getwd()
setwd("C:/Users/Mal/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/")

#this command creates a list based on the pattern: used http://regexr.com/ to test expression before using
file_list <- list.files(pattern = '.*A18*......5-24.*')
datalist <- lapply(file_list, function(x){read.table(file=x)})
Reduce(function(x,y) {merge(x,y)}, datalist)


for (file in file_list){
  dataset <- read.table(file)
  dat
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, header=TRUE, sep="\t")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.table(file, header=TRUE, sep="\t")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}


library(dplyr)
library(readr)
df <- list.files(pattern = '.*A18*......5-24.*') %>% 
  lapply(read_table) %>% 
  bind_rows 