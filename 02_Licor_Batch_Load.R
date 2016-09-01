#Author: Dave Moore
#Modified: Mallory Barnes
  #Date: 01/05/2016 
  #Purpose: Script for reading in LiCor files, doing QC, fitting A/Ci. 
  #Then creates summary df for work with stats
#Here's some useful info on QC with Pecan Photosynthesis: https://github.com/PecanProject/pecan/blob/master/modules/photosynthesis/vignettes/ResponseCurves.Rmd
  
  
#Grab the following packages before you start
  
library(devtools)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(ggplot2)
library(PEcAn.photosynthesis)

  
  ## Get list of LI-COR 6400 file names (ASCII not xls)
  
  #this command looks for files *starting with* '2015-01-' within the folder "data" - the files adhere to 
  # a naming convention that let's the code pull out whether the file is an A/ci curve or an aq curve
  
  setwd("C:/Users/rsstudent/Dropbox/")
  path_files <- "Summer_2016_Drought_Experiment/A_Ci/"
  filenames <- dir(path_files)
  filenames <- filenames[-grep(".xls", filenames, fixed=T)]
  fileswithpath=paste0(path_files, filenames)
    ## Load files to a list
  master = lapply(fileswithpath, read.Licor)
    
    
      # # The code below performs a set of interactive QA/QC checks on the LI-COR data that's been loaded. 
      # Because it's interactive it isn't run when this vignette is knit.
      
      # # If you want to get a feel for how the code works you'll 
      # want to run it first on just one file, rather than looping over all the files
      
      #this runs licor QC on a file - you click on outliers to remove them
      master[[1]] <- Licor.QC(master[[1]])
      
        #same process for all files - this makes the first image pop up. 
        for(i in 1:length(master)){
            master[[i]] = Licor.QC(master[[i]])
          }
      
        #after the QC process combine the files into one data frame
        
        #This is what I'm supposed to use based on the pecan documentation but that was giving
        #me the unequal columns error so I used "bind_rows" from dply r instead: 
        #dat<-do.call("rbind", master)
        
        dat <- bind_rows(master)
        
        #If you want both the bad and good observations in a data file, you'll need to save it here: 
        
        write.csv(dat, "QC_9_1_2016_bad_and_good.csv")
        

          
          ## if QC was done, remove both unchecked points and those that fail QC
          if("QC" %in% colnames(dat)){
              dat = dat[-which(dat$QC < 1),]  
            } else{
                QC = rep(1,nrow(dat))
                dat = cbind(dat,QC)
              }
#write "dat" to a .csv file so I don't have to do QA/QC again: 

write.csv(dat, "QC_9_1_2016.csv")

#Status 
        