#Author: Dave Moore
#Modified: Mallory Barnes
#Date: 01/05/2016 
#Purpose: Script for reading in LiCor files, doing QC, fitting A/Ci. 
#Then creates summary df for work with stats

#Grab the following packages before you start

library(devtools)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)


###### Install Pecan: Only need to do this once ########
#installing PEcAn.photosynthesis as a stand alone
#need rjags installed in R and also JAGS (stand alone application) installed on your computer
#Jags download can be found here: http://www.sourceforge.net/projects/mcmc-jags/files
#You will then download an html file which will take you to where you can download one of two .exe files
#Can use command "sessionInfo()" to see which version of R you're using. 
#need Rtools installed

if (!require("PEcAn.photosynthesis",character.only = TRUE))
{
  library(devtools)
  install_github("PecanProject/pecan/modules/photosynthesis") 
}

#note you will get an error: it says "In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
#there is no package called ‘PEcAn.photosynthesis’"" 

library(PEcAn.photosynthesis)

######  Install PlantEcoPhys ######
  # Plantecophys - An R Package for Analysing and Modelling Leaf Gas Exchange Data
  # Remko A. Duursma
  # Paper describing the package: http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0143346
  # Source code and descriptions: https://bitbucket.org/remkoduursma/plantecophys

install_bitbucket("remkoduursma/plantecophys")
