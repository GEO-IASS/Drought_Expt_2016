#Author: Dave Moore
#Modified: Mallory Barnes
  #Date: 01/05/2016 
  #Purpose: Script for reading in LiCor files, doing QC, fitting A/Ci. 
  #Then creates summary df for work with stats
  
  
  #Grab the following packages before you start
library(devtools)
library(PEcAn.photosynthesis)


#### To load a single file using PECAN.photosynthesis
  ## Read LI-COR 6400 files (ASCII not xls)
#Working across multiple computers so need to set working directory: 
#For USDA Computer: 
setwd("C:/Users/rsstudent/Dropbox/Summer_2016_Drought_Experiment/A_Ci/")


Single_File <- read.Licor("b2_pop_a18_aci_mb_06_30_2016")

?read.Licor

