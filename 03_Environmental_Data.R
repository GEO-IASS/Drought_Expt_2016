#Author: Dave Moore
#Modified: Mallory Barnes
#Date: 01/07/2016 
#Purpose: Script for reading in LiCor files, doing QC, fitting A/Ci. 
#Then creates summary df for work with stats


#Grab the following packages before you start
library(devtools)
library(PEcAn.photosynthesis)
library(plantecophys)

#### To load a single file using PECAN.photosynthesis
## Read LI-COR 6400 files (ASCII not xls)

#Prompt - program A/Ci curves for plant ID so that there's a column with PlantID in all licor files
#Pecan Function for A/Ci QC 
master <- Licor.QC(NameofDataFrame)

#Merge into 1 data frame once QC checked
dat<-do.call("rbind", master)

#Fit Model to Composite Data - Do I even want to do this? 
fit1 <- fitA(master[[1]])

#Filter out -1 QC points here 

#Plant_Ecophys
fitaci(NameofDataFrame)
plot(fitaci(NameofDataFrame))

#Dave's stuff----------------------------------------------------
APari_plant1 = read.Licor("./data/Light/2016-01-05-ots-light-curve-plant-1")
AACI_plant1 = read.Licor("./data/ACI/2016-01-05-ots-aci-curve-plant-1")


####ENvironmental Data
ENVdata1 <- read.Licor("./data/ots-2016-06-01-env-plant-1")
ENVdata2 <- read.Licor("./data/ots-2016-06-01-env-plant-2")
ENVdata3 <- read.Licor("./data/ots-2016-06-01-env-plant-3")
ENVdata4 <- read.Licor("./data/ots-2016-06-01-env-plant-4")
ENVdata5 <- read.Licor("./data/ots-2016-06-01-env-plant-5")
ENVdata6 <- read.Licor("./data/ots-2016-06-01-env-plant-6")

#Shade Plants
PAR_plant1 = mean(ENVdata1$PARo)
PAR_plant2 = mean(ENVdata2$PARo)
PAR_plant3 = mean(ENVdata3$PARo)

SunPAR = mean(PAR_plant1,PAR_plant2,PAR_plant3)
plot(SunPAR)

#Sun Plants
PAR_plant4 = mean(ENVdata4$PARo)
PAR_plant5 = mean(ENVdata5$PARo)
PAR_plant6 = mean(ENVdata6$PARo)

ShadePAR = mean(PAR_plant4,PAR_plant5,PAR_plant6)
plot(ShadePAR)
