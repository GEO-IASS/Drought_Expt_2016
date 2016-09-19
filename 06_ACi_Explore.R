#Author: Mallory Barnes
#Date: 08/13/2016 
#Input: QA/QC-ed A/CI Curves, climate/water potential data
#Output: merged dataframe with all parameters (Vcmax, Jmax) merged to proper date/time/waterpot

#Necessary Pacakges: 
library(lubridate)
library(ggplot2)
#Load files
getwd()
A_Ci_ests <- read.csv("C:/Users/Mallory/Dropbox/Estimates_post_400_removal.csv")
Climate_data <- read.csv("C:/Users/Mallory/Dropbox/Summer_2016_Drought_Experiment/Prelim_Alec2.csv")


#Going to have to create a "uniqueID" for each observation (with plant Date and Time)
#In order to properly merge the two datasets. Could potentially do this by hand as well. Maybe 
#would be easier in fact. 

#Steps for Cliamte file: parse date from the climate_file
#                        Create uniqueID from the date and Plant_ID (uncapitalize plant IDS)

str(Climate_data)
Climate_data$Date <- as.Date(Climate_data$Date, "%m/%d/%Y")
Climate_data$Plant_ID <- tolower(Climate_data$Plant_ID)
Climate_data$uniqueID <- paste(Climate_data$Plant_ID, Climate_data$Date, sep='-')


#Steps for ACI file:    parse date and plant ID from ACIs from column "fname"
#                       Create uniqueID from the date and Plant_ID

str(A_Ci_ests)

#Custom function for pulling x number of characters from the right of a string: from 
#http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r

substrRight <- function(x, n){
        substr(x, nchar(x)-n+1, nchar(x))
}



A_Ci_ests$Date <- substrRight(as.character(A_Ci_ests$fname), 9)
A_Ci_ests$Date <-as.Date(A_Ci_ests$Date, "%m_%d_%Y")
A_Ci_ests$Plant_ID <- substr(as.character(A_Ci_ests$fname), 8,10)
A_Ci_ests$uniqueID <- paste(A_Ci_ests$Plant_ID, A_Ci_ests$Date, sep='-')

#Initial merge - a lot didn't take. 

write.csv(A_Ci_ests, "A_Ci_Ests_to_merge.csv")
write.csv(Climate_data, "Climate_data_to_merge.csv")
merged <-merge(A_Ci_ests, Climate_data, by="uniqueID")

write.csv(merged, "Merged_data_to_analyze.csv")

str(merged)
#Initial Plotting-------------------
ggplot(merged, aes(Water_Pot, Vcmax, colour=Genotype)) + geom_line(aes(group=Plant_ID.x))
ggplot(merged, aes(Date.x, Vcmax, colour=Plant_ID.x)) + geom_line(aes(group=Plant_ID.x))


#Checking out correlations------------------
require(plyr)
func <- function(xx)
{
        return(data.frame(COR = cor(xx$VPD, xx$Vcmax)))
}

ddply(merged, .(Genotype), func)
ddply(merged, .(Plant_ID.x), func)

func2 <- function(xx)
{
        return(data.frame(COR = cor(xx$max, xx$wind_speed)))
}


ddply(merged, .(Genotype), func2)
ddply(merged, .(Plant_ID.x), func2)

str(merged)
qplot(merged$Vcmax, merged$Date.x)

#There is variation in Vcmax and Jmax over the season but it does not seem to be explained by 
#Water potential (stress). It does, however, seem to be explained - to some extent, by VPD