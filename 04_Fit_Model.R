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
dat <- read.csv("C:/Users/Mallory/Dropbox/QC_3_5_2017.csv")

#Format dat$fname as factor
dat$fname <- as.factor(dat$fname)

#Originally did it the hard way until I found "fitacis":
#by(dat, dat$fname, fitaci)

#Fit curves to all A/Ci files
fits <- fitacis(dat, "fname")

## Trying to figure out 'Photosyn' to find Ci/Ca ratio##
plot(Photosyn(VPD = 1.5, Ca = 400, PPFD = 1500, Tleaf = 25, Patm = 100,
         RH = NULL, gsmodel = c("BBOpti", "BBLeuning", "BallBerry"), g1 = 4,
         g0 = 0, gk = 0.5, vpdmin = 0.5, D0 = 5, GS = NULL, alpha = 0.24,
         theta = 0.85, Jmax = 100, Vcmax = 50, gmeso = NULL, TPU = 1000,
         Rd0 = 0.92, Q10 = 1.92, Rd = NULL, TrefR = 25, Rdayfrac = 1,
         EaV = 82620.87, EdVC = 0, delsC = 645.1013, EaJ = 39676.89,
         EdVJ = 2e+05, delsJ = 641.3615, GammaStar = NULL, Km = NULL,
         Ci = NULL, Tcorrect = TRUE, returnParsOnly = FALSE, whichA = c("Ah",
                                                                        "Amin", "Ac", "Aj")))
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
#vcmax_jmax$PlantID -> substr(as.character(vcmax_jmax$fname),8,10)
#substr(vcmax_jmax$fname,8,10)
str(vcmax_jmax)
write.csv(vcmax_jmax, "Estimates_3_5_2017.csv")
plot(vcmax_jmax$Jmax, vcmax_jmax$Jmax_SE)

to_review <- subset(vcmax_jmax, Jmax_SE >5)
