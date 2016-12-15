library(plyr)
library(psych)
library(Hmisc)
merged <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/Merged_data_to_analyze.csv")
hyperspec <-read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/Processed_Hyperspec_Files.csv")

head(merged)
merged[25:35,]
head(hyperspec)


#hyperspectral data - create "unique id" (same as in 'merged')and then average by unique ID
#Then will be able to merge with A/Ci and Climate files
#'uniqueID' takes form: a18-2016-06-08 

hyperspec$uniqueID <- paste(tolower(hyperspec$ID), hyperspec$date, sep='-')

#want means of VIs by uniqueID - use ddply
hyperspec_avg <- ddply(hyperspec,~uniqueID,summarise, NDVI=mean(NDVI), PRI=mean(PRI), NDWI=mean(NDWI))

#merge hyperspec and a/ci data

all_data <- merge(hyperspec_avg, merged, by="uniqueID")
str(all_data)
write.csv(all_data, "C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data.csv")
#clean up "all_data"
#delete X.2, X.1, and X columns (what even are these?)

all_data <- subset(all_data, select=-c(X,X.1, X.2))

#giant correlation matrix!
c <-(cor(all_data[,unlist(lapply(all_data, is.numeric))]))
write.table(c, "clipboard", sep="\t", row.names=FALSE)

all_data <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/all_data.csv")
#1) Are Vcmax and Jmax Sensistive to Drought Stress?---------------------
#Function to return correlations by group

all_data$Date.x <- as.Date(all_data$Date.x)
all_data
phase_1 <-subset(all_data, Date.x<="2016-06-08")
phase_2 <- subset(all_data, Date.x>="2016-06-08" & Date.x<="2016-06-16")
phase_3 <- subset(all_data, Date.x>="2016-06-16")
require(plyr)
func <- function(xx, a, b)
{
        return(data.frame(COR = cor(xx$NDWI, xx$Water_Pot)))
}

ddply(phase_3, .(Genotype), func)

ggplot(data=all_data,
       aes(x=Delta_T, y=Vcmax, colour=Genotype)) +
        geom_point()+
        geom_smooth()
#Finding good comparison observations: Looks like Individual E4, E10, E3 and G11 all got pretty stressed
#Gonna subset and re-plot

merged_stressed <- subset(merged, Plant_ID.x == "e04" | Plant_ID.x == "e03" | Plant_ID.x == "e10" | Plant_ID.x == "g11")

ggplot(merged_stressed, aes(Date.x, Water_Pot, colour=Plant_ID.x)) + geom_line(aes(group=Plant_ID.x))

#Let's look at E03 on 6/24 (stressed) and E03 on 06/09 (not stressed)
#Cool but how to 

#Get and Plot Hyperspec Files-------------------
#for my personal laptop
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")

not_stressed <- read.table("ASCII_Reflectance/b2popmlb_E3_Leaf_6-01-201600000.asd.txt", col.names=c("wavelength", "reflectance"))
str(not_stressed)

stressed <- read.table("ASCII_Reflectance/b2popmlb_E03_leaf_6-24-201600001.asd.txt", col.names=c("wavelength", "reflectance"))

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

not_stressed <- Format_hyperspec(not_stressed)
stressed <- Format_hyperspec(stressed)
#Simple Plot of Test
str(not_stressed)
str(stressed)


p <- ggplot() +
        # blue plot
        geom_point(data=not_stressed, aes(x=wavelength, y=reflectance), colour="blue")  +
        #geom_smooth(data=not_stressed, aes(x=wavelength, y=reflectance), fill="blue",
        #colour="darkblue", size=1) +
        # red plot
        geom_point(data=stressed, aes(x=wavelength, y=reflectance, colour="red")) 

#geom_smooth(data=stressed, aes(x=wavelength, y=reflectance), fill="red",
#colour="red", size=1)
p

#Merge the two datafames together to make graph nicer: 

merged_e3 <- merge(stressed, not_stressed, by="wavelength")
str(merged_e3)
