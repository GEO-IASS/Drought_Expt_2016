library(ggplot2)
library(hyperSpec)

merged <- read.csv("C:/Users/Mal/Dropbox/Drought_Expt_2016/Merged_data_to_analyze.csv")

#Water Potential Data--------------------
ggplot(merged, aes(Date.x, Water_Pot, colour=Plant_ID.x)) + geom_line(aes(group=Plant_ID.x))

#Finding good comparison observations: Looks like Individual E4, E10, E3 and G11 all got pretty stressed
#Gonna subset and re-plot

merged_stressed <- subset(merged, Plant_ID.x == "e04" | Plant_ID.x == "e03" | Plant_ID.x == "e10" | Plant_ID.x == "g11")

ggplot(merged_stressed, aes(Date.x, Water_Pot, colour=Plant_ID.x)) + geom_line(aes(group=Plant_ID.x))

#Let's look at E03 on 6/24 (stressed) and E03 on 06/09 (not stressed)

#Get and Plot Hyperspec Files-------------------
#for my personal laptop
setwd(dir = "C:/Users/Mal/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")

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

