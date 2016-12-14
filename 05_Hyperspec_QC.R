#Author: Mallory Barnes
#Date: 09/5/2016 
#Purpose: Quality check Hyperspectral Files for any obvious outliers 
#Input: Hyperspectral  Reflectances in ASCII format
#Output: Quality Checked Hyperspectral Reflectances 

library(ggplot2)
library(hyperSpec)
#hyperSpec manual can be found here: https://cran.r-project.org/web/packages/hyperSpec/vignettes/introduction.pdf

#Set working directory to location fo hyperspectral ASD Files
getwd()
#for my personal laptop
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")
#for my ARS computer
setwd(dir = "C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")

#Read in file, used "col.names" argument to rename columns properly. Will need to figure out
#how to do this with a list of ASCII files
test <- read.table("ASCII_Reflectance/b2popmlb_A1_Leaf_5-20-201600000.asd.txt", col.names=c("wavelength", "reflectance"))
str(test)

#Create Column with filename and then delete the first row of data frame
test$filename <- (test$reflectance[1:1])
test = test[-1,]

#Change factors to numeric
test$wavelength <- as.numeric(levels(test$wavelength))[test$wavelength]
test$reflectance <-as.numeric(levels(test$reflectance))[test$reflectance]

str(test)
head(test)

#Simple Plot of Test
qplot(test$wavelength, test$reflectance)

#Test Calculate Indices

filename <- substr(test[1,3], 1,31)
ID <-  substr(test[1,3], 10,11)
date <- (substr(test[1,3], 18,26))
observation <- (substr(test[1,3], 30,31))
PRI <- ((test[182,2]-test[221,2])/(test[182,2]+test[221,2]))
NDVI <- ((test[511,2]-test[341,2])/(test[511,2]+test[341,2]))
NDWI <- ((test[511,2]-test[891,2])/(test[511,2]+test[891,2]))


test_df <- cbind(filename, ID, date, observation, PRI, NDVI, NDWI)

test[182,2]
test[221,2]
test[511,2]
test[341,2]

#For Loop -----------------------------------------------------------

# Define function to calculate mean from instrument output data file
PRI = function(w_531, w_570){((w_531-w_570)/(w_531+w_570))}
NDVI = function(w_860, w_690){((w_860-w_690)/(w_860+w_690))}
NDWI = function(w_860, w_1240){((w_531-w_570)/(w_531+w_570))}



# batch import text files (files must be in working directory); 'pattern' is case-sensitive
txtfiles = list.files(pattern="*.TXT")

means = numeric(length(txtfiles))

# Loop that creates subset (SS, which is 10-63 micron range), assigns name to columns, and 
# computes mean of that subset for all the text files that were read in

for (i in 1:length(txtfiles)){
  tmp = read.table(txtfiles[i], sep=",", skip=15)
  SS = data.frame(subset(tmp, V1 > 9.99)) 
  diam = SS[[1]]
  freq = SS[[2]]
  means[i] = meanSS(diam, freq)
}

# define vectors to store results (mean sortable silt values)
results = data.frame(txtfiles, means)
print(results)

#having trouble getting it to properly read the txt files in the folder I tell it to, instead am having
#to run the function on the list of text files. 
source ("scan.txt.Poplar.R")
scan.txt.Poplar("ASCII_Reflectance/")

list.files("ASCII_Reflectance/", pattern = "*.txt")
