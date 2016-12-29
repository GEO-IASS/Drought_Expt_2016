#Author: Mallory Barnes
#Date: 12/29/2016
#The purpose of this code is to add addtional licor files to the big dataset I'm using for analysis: 
#currently located in: all_data <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data.csv") in the 
#09_merged_and_analysis file in this Rstudio project 
#Two steps: 
#1) Get the first observation of "photo", "cond", and "Trrmmol" files into a dataframe for analysis
#2) Merge these files with the 'all_data' file
#Input: Non-QA-QCed Licor files containing all licor observations, 'all_data' file (step 2)
#Output: Big data file containing the additional licor variables

#load Licor files and get first observation for each: 

Licor_files <- read.csv("C:/Users/Mallory/Dropbox/QC_9_1_2016_bad_and_good.csv")

#Quandry! First observation is what we want, however, several have a QC of '-1' so debating whether
#to use the 2nd observation if the first observation is of poor quality, or just use the 1st observation 
#no matter what - seems like the latter is a better plan because the 2nd point won't even be indicative 
#Keep the QC flag in case it ends up being relevant

str(Licor_files)

#Subset relevant variables: 
Licor_subset <- subset(Licor_files, select=c('fname', 'Obs', 'HHMMSS', 'Trmmol', 'Cond', 'Photo'))

#Need to get 'Plant_ID' and 'Date' properly formatted 
#Some of the filnames have dashes instead of underscores - to fix: 
Licor_subset$fname <- gsub('-','_', Licor_subset$fname)

Licor_subset$Plant_ID <- substr(Licor_subset$fname, 8,10)
#Licor_date <- substr(Licor_subset$fname, 19,28)
Licor_date <- as.Date(substr(Licor_subset$fname, 19,28), "%m_%d_%Y")

#Funky dates: obs: 430-442, 704-716
#Problem - dashes instead of underscores (Ugh!)
#I'll have to change manually


Licor_date[430:442,]

Licor_date
