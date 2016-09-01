#Author: Mallory Barnes
#Date: 08/13/2016 
#Input: QA/QC-ed A/CI Curves
#Output: Data frame with the outputs of fit_ACI from plant_ecophys

#Necessary Pacakges: 
library(devtools)
library(PEcAn.photosynthesis)
library(plantecophys)

#Purpose: fitting A/Ci curves to files using Plant_Ecophys. 
#Then create summary df for work with stats

#Load up saved datafile
dat <- read.csv("QC_9_1_2016.csv")

#Format dat$fname as factor
dat$fname <- as.factor(dat$fname)

#Originally did it the hard way until I found "fitacis":
#by(dat, dat$fname, fitaci)

#Fit curves to all A/Ci files
fits <- fitacis(dat, "fname")

#Plot Vcmax by Jmax
with(coef(fits), plot(Vcmax, Jmax))

#Extract 1 curve: 
fits[[1]]
plot(fits[[1]])

#Plot all curves separately
plot(fits)

#Plot all curves in `1 plot
plot(fits, how="oneplot")

#Can summarize elements using sapply
rmses <- sapply(fits, "[[", "RMSE")

#Plot worst fitting curve
plot(fits[[which.max(rmses)]])


#This is what we want for analysis: 
vcmax_jmax <- coef(fits)

#Parse filname to get Plant ID and Date using substr
vcmax_jmax$fname <- as.character(vcmax_jmax$fname)
vcmax_jmax$PlantID -> substr(vcmax_jmax$fname, 2,3)

vcmax_jmax
write.csv(vcmax_jmax, "Estimates_post_400_removal.csv")
plot(vcmax_jmax$Jmax, vcmax_jmax$Jmax_SE)

to_review <- subset(vcmax_jmax, Jmax_SE >5)
