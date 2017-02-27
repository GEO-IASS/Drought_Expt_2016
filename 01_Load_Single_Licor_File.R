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
setwd("C:/Users/rsstudent/Dropbox/Summer_2016_Drought_Experiment/A_Ci/A_Ci_Redo_2_23/")


Single_File <- read.Licor("b2 pop H9 mb aci 5-26-2016")

?read.Licor

