#Author: Mallory Barnes
#Date: 1/31/2017
#Purpose: Format files for use in PLSRopt package
#Input: Hyperspectral  Reflectances in ASCII format
#Output: Single data frame in following format: 
peach <- read.csv("")

# UniqueID Vcmax Jmax Water_Pot  300 301 302 303 etc. etc. (data must be in wide format)
#for my personal laptop
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")
setwd(dir = "C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")

#Read in file, used "col.names" argument to rename columns properly. Will need to figure out
#how to do this with a list of ASCII files
#Testing reshape with a single file 'test'
test <- read.table("C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/b2popmlb_A01_leaf_5-20-201600000.asd.txt", col.names=c("wavelength", "reflectance"))
str(test)
test$filename <- basename("C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/b2popmlb_A01_leaf_5-20-201600000.asd.txt")
test_wide <- reshape(test, idvar="filename", timevar="wavelength", direction="wide")



#Lapply to calculate for all hyperspec files -----------------------------------------------------------

#create list of text files (files must be in working directory); 'pattern' is case-sensitive
setwd(dir = "C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")
textfiles = list.files("ASCII_Reflectance/", pattern = "*.txt")
#txtfiles_subset is to test out the lapply
textfiles_subset = textfiles[1:15]

setwd(dir = "C:/Users/rsstudent/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/ASCII_Reflectance/")

#Function to format in wide format
format_PLSR <- function(x){
        tmp = read.table(x,  col.names=c("wavelength", "reflectance"))
        tmp$filename <- basename(x)
        tmp = tmp[-1,]
        tmp$wavelength <- as.numeric(levels(tmp$wavelength))[tmp$wavelength]
        tmp$reflectance <-as.numeric(levels(tmp$reflectance))[tmp$reflectance]
        filename <- substr(tmp[1,3], 1,40)
        print(filename)
        ID <-  substr(tmp[1,3], 10,12)
        date <- (substr(tmp[1,3], 19,27))
        observation =(substr(tmp[1,3], 31,32))
        reflectances <- reshape(test, idvar="filename", timevar="wavelength", direction="wide")
        indices=as.data.frame(cbind(filename, ID, date, observation, reflectances))
        return(indices)
}

#Lapply "calc_indices functinon" over subset of files: subset of 15 files takes 30 seconds
indices_tmp <- lapply(textfiles_subset, format_PLSR)
#Lapply "calc_indices functinon" over full list of files: takes awhile - 34 minutes for all files
indices_tmp <- lapply(textfiles, format_PLSR)

#Bind rows together: this can also take awhile - about 15 minutes for all files 
indices <- do.call(rbind, indices_tmp)
write.csv(indices, "C:/Users/rsstudent/Dropbox/Drought_Expt_2016/hyperspec_for_PLSR.csv")

#str still looks really funky
str(indices)
indices[200:235,]
head(indices)
#Going to format different variables now (numeric, date, etc. etc. for analysis)
indices$PRI <- as.numeric(as.character(indices$PRI))
indices$NDVI <- as.numeric(as.character(indices$NDVI))
indices$NDWI <- as.numeric(as.character(indices$NDWI))
indices$date <- as.Date(indices$date, format="%m-%d-%Y")

write.csv(indices,"C:/Users/rsstudent/Dropbox/Drought_Expt_2016/Processed_Hyperspec_Files.csv")


# define vectors to store results (mean sortable silt values)
results = data.frame(txtfiles_subset, indices)
print(results)




