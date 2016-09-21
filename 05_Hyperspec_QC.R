#Author: Mallory Barnes
#Date: 09/5/2016 
#Purpose: Quality check Hyperspectral Files for any obvious outliers 
#Input: Hyperspectral  Reflectances in ASCII format
#Output: Quality Checked Hyperspectral Reflectances 

library(ggplot2)
library(hyperSpec)
#hyperSpec manual can be found here: https://cran.r-project.org/web/packages/hyperSpec/vignettes/introduction.pdf

#Set working directory to location fo hyperspectral ASD Files
getwd()
#for my personal laptop
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")
#for my ARS computer
setwd(dir = "C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")

#Read in file, used "col.names" argument to rename columns properly. Will need to figure out
#how to do this with a list of ASCII files
test <- read.table("ASCII_Reflectance/b2popmlb_A1_Leaf_5-20-201600000.asd.txt", col.names=c("wavelength", "reflectance"))
str(test)

#Create Column with filename and then delete the first row of data frame
test$filename <- (test$reflectance[1:1])
test = test[-1,]

#Change factors to numeric
test$wavelength <- as.numeric(levels(test$wavelength))[test$wavelength]
test$reflectance <-as.numeric(levels(test$reflectance))[test$reflectance]

str(test)
head(test)

#Simple Plot of Test
qplot(test$wavelength, test$reflectance)

#Create Hyperspec object from 1 file
test_spec <- new ("hyperSpec", spc = test$reflectance, wavelength = test$wavelength, label=test$filename)

#Check out dimensions
test_spec
summary(test_spec)
nrow(test_spec)
nwl(test_spec)
ncol(test_spec)
dim(test_spec)

#plotting
plot(test_spec, "spcprctile")
plotspc(test_spec)

#Create new hyperSpec object and custom ASCII import function-------------------------------------
#https://cran.r-project.org/web/packages/hyperSpec/vignettes/fileio.pdf see this Vignette for help (page 13)
vignette ("fileio")

scan.txt.Poplar <- function (files = "*.txt", label = list (),
                                  short = "scan.txt.Poplar", user = NULL, date = NULL) {
        ## set some defaults
        long <- list (files = files, label = label)
        label <- modifyList (list (.wavelength = expression (lambda / nm),
                                   spc = expression (I[fl] / "a.u.")),
                             label)
        ## find the files
        files <- Sys.glob (files)
        if (length (files) == 0){
                warning ("No files found.")
                return (new ("hyperSpec"))
        }
        
        ## read the first file
        buffer <- matrix (read.table(files [1], col.names=c("wavelength", "reflectance")))
        ## first column gives the wavelength vector

        #convert both to numeric
        buffer$wavelength <- as.numeric(levels(buffer$wavelength))[buffer$wavelength]
        buffer$reflectance <-as.numeric(levels(buffer$reflectance))[buffer$reflectance]
        wavelength <- buffer$wavelength
        ## preallocate the spectra matrix:
        ## one row per file x as many columns as the first file has
        spc <- matrix (ncol = nrow (buffer), nrow = length (files))
        ## the first file's data goes into the first row
        spc [1, ] <- buffer [, 2]
        ## now read the remaining files
        for (f in seq (along = files)[-1]) {
                buffer <- matrix (scan (files [f]), ncol = 2, byrow = TRUE)
                ## check whether they have the same wavelength axis
                if (! all.equal (buffer [, 1], wavelength))
                        stop (paste(files [f], "has different wavelength axis."))
                spc [f, ] <- buffer[, 2]
        }
        ## make the hyperSpec object
        new ("hyperSpec", wavelength = wavelength, spc = spc,
             data = data.frame (file = files), label = label,
             log = list (short = short, long = long, user = user, date = date))
}

#having trouble getting it to properly read the txt files in the folder I tell it to, instead am having
#to run the function on the list of text files. 
source ("scan.txt.Poplar.R")
scan.txt.Poplar("ASCII_Reflectance/")
list.files("ASCII_Reflectance/", pattern = "*.txt")
