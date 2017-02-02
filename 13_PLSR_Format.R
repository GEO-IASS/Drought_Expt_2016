#Author: Mallory Barnes
#Date: 1/31/2017
#Purpose: Format files for use in PLSRopt package
#Input: Hyperspectral  Reflectances in ASCII format
#Output: Single data frame in following format: 

# UniqueID Vcmax Jmax Water_Pot  300 301 302 303 etc. etc. (data must be in wide format)
#for my personal laptop
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")

#Read in file, used "col.names" argument to rename columns properly. Will need to figure out
#how to do this with a list of ASCII files
test <- read.table("ASCII_Reflectance/b2popmlb_A1_Leaf_5-20-201600000.asd.txt", col.names=c("wavelength", "reflectance"))
str(test)

#Lapply to calculate for all hyperspec files -----------------------------------------------------------

#create list of text files (files must be in working directory); 'pattern' is case-sensitive
setwd(dir = "C:/Users/Mallory/Dropbox/Mallory_Hyperspectral/9_2_2016_hyperspectral/")
textfiles = list.files("ASCII_Reflectance/", pattern = "*.txt")
#txtfiles_subset is to test out the lapply
textfiles_subset = txtfiles[1:5]

#Function to format in wide format
calc_indices <- function(x){
        tmp = read.table(x,  col.names=c("wavelength", "reflectance"))
        tmp$filename <- basename(x)
        tmp = tmp[-1,]
        tmp$wavelength <- as.numeric(levels(tmp$wavelength))[tmp$wavelength]
        tmp$reflectance <-as.numeric(levels(tmp$reflectance))[tmp$reflectance]
        filename <- substr(tmp[1,3], 1,40)
        ID <-  substr(tmp[1,3], 10,12)
        date <- (substr(tmp[1,3], 19,27))
        observation =(substr(tmp[1,3], 31,32))
        w_531 =tmp[182,2]
        w_570 = tmp[221,2]
        w_690 = tmp[341,2]
        w_860 = tmp[511,2]
        w_1240 = tmp[891,2]
        PRI = calc_PRI(w_531, w_570)
        NDVI = calc_NDVI(w_860, w_690)
        NDWI = calc_NDWI(w_860, w_1240)
        indices=as.data.frame(cbind(filename, ID, date, observation, PRI, NDVI, NDWI))
        return(indices)
}


#Lapply "calc_indices functinon"
indices_tmp <- lapply(textfiles, calc_indices)
#Bind rows together
indices <- do.call(rbind, indices_tmp)
#str still looks really funky
str(indices)
indices[200:235,]
head(indices)
#Going to format different variables now (numeric, date, etc. etc. for analysis)
indices$PRI <- as.numeric(as.character(indices$PRI))
indices$NDVI <- as.numeric(as.character(indices$NDVI))
indices$NDWI <- as.numeric(as.character(indices$NDWI))
indices$date <- as.Date(indices$date, format="%m-%d-%Y")

write.csv(indices,"C:/Users//Mallory/Dropbox/Drought_Expt_2016/Processed_Hyperspec_Files.csv")


# define vectors to store results (mean sortable silt values)
results = data.frame(txtfiles_subset, indices)
print(results)




