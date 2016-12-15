library(ggplot2)
library(lubridate)

#See what the working directory is 
getwd()
setwd("C:/Users/Mallory/Dropbox/Summer_2016_Drought_Experiment/")

Predawns <- read.csv("Pre_dawns.csv")

str(Predawns)
Predawns$Date <- as.Date(Predawns$Date,  "%m/%d/%Y")

str(Predawns)

Predawns$uniqueID <- paste(tolower(Predawns$PlantID), Predawns$Date, sep='-') 

hyperspec <-read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/Processed_Hyperspec_Files.csv")

str(hyperspec)

hyperspec$uniqueID <- paste(tolower(hyperspec$ID), hyperspec$date, sep='-')

#want means of VIs by uniqueID - use ddply
hyperspec_avg <- ddply(hyperspec,~uniqueID,summarise, NDVI=mean(NDVI), PRI=mean(PRI), NDWI=mean(NDWI))

phase_1_2 <- merge(Predawns, hyperspec_avg, by="uniqueID")

Phase_2 <- subset(phase_1_2, Phase==2)

func <- function(xx, a, b)
{
        return(data.frame(COR = cor(xx$NDVI, xx$Water.Potential)))
}

ddply(Phase_2, .(Genotype), func)

ggplot(Predawns, aes(Date, Water.Potential)) + geom_point(aes(colour=factor(Genotype)), size=3)

ggplot(Predawns, aes(Date, Water.Potential, colour=PlantID)) + geom_point(size=3) 

R_270 <-Predawns[ which(Predawns$Genotype =="R-270"),]

Gen_57 <-Predawns[ which(Predawns$Genotype =="57-276"),]


ggplot(R_270, aes(Date, Water.Potential, colour=PlantID)) + geom_point(size=3) + geom_line()

ggplot(Gen_57, aes(Date, Water.Potential, colour=PlantID)) + geom_point(size=3) +geom_line()

