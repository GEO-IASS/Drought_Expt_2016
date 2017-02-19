#Author: Mallory Barnes
#Date: 2/17/2017 
#Purpose: create R-squared varaiance figure using plsropt package
#Figure shows variance around R-squared (10 different random samples) given different proportions
#of training vs. testing data. 
#Input: .csv file containing full leaf specta in wide format
#Output: ggplot figure: shows spread of R-squared values for various proportions of testing vs. training 
#data for this dataset


library(pls)
library(plyr)
library(devtools)
library(signal)
#If plsropt is not installed: use command 'install_github("uwadaira/plsropt", dependencies = TRUE)'
library(plsropt)
library(gdata)
library(progress)

#Load wide format hyperspectral data
poplar_names <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths.csv")
str(poplar_names)
#Format date properly
poplar_names$date.y <- as.Date(poplar_names$date.y)

#When subsetting by genotype: 
#poplar_names <- subset(poplar_names, Genotype=="R-270")
#or
#poplar_names <-subset(poplar_names, Genotype=="52-276")

#When ordering by water potential (descending)
#poplar_names <- poplar_names[order(-poplar_names$Water_Pot),] 

#When ordering by date (descending)
#poplar_names <- poplar_names[order(poplar_names$date.y),] 
#poplar_names$date.y

#get rid of 'x' column, Water_Pot, and Genotype for now 
poplar_names <- poplar_names[ -c(1, 6:7)]
#change col names (all say 'x' right now(?))
names(poplar_names) <- gsub("X", "", names(poplar_names))
str(poplar_names)
#get rownames of all data: 'unique_ID" containing plant ID and date
poplar_all <- poplar_names[,-1]
rownames(poplar_all) <- poplar_names[,1]
rownames(poplar_all)

#check column names
colnames(poplar_all)[1:5]
#check last five column names
colnames(poplar_all)[(ncol(poplar_all)-4):ncol(poplar_all)]

#extract x variables (350 nm - 2500 nm)
x <-extdat(poplar_all, start=350,end=2500)
poplar<-data.frame(poplar_all[,1:2], NIR=I(as.matrix(x)))
#extracting range of 700 nm to 1098 nm
poplar$NIR<-extdat(poplar$NIR,start=350,end=2500)
#preprocessing: standard normal variate
poplar$NIR <- snv(poplar$NIR)
#Savitky-Golay second derivative
poplar$NIR <- matsgolay(poplar$NIR, p=2, n=11, m=2)
#Auto-scaling
poplar$NIR <- scale(poplar$NIR, center = TRUE, scale = TRUE)
#Divide data set into training and test set
#For non-randomzed purposes
#datTrain <- poplar[-c(1:18),]
#datTest <- poplar[19:28,]

#This is for only obs on June 23rd and 24th
#datTrain <- poplar[-c(25:33),]
#datTest <- poplar[25:33,]

#This is for only obs on June 23rd and 24th for GT 52-276
#datTrain <- poplar[-c(13:17),]
#datTest <- poplar[13:17,]

str(poplar)
#Shuffle Rowwise: 
poplar_rand <- poplar[sample(nrow(poplar)),]
#Break into training and testing set based on 'cutoff'
cutoff = round(0.3*nrow(poplar_rand))
datTrain <- poplar_rand[1:cutoff,]
datTest <- poplar_rand[-(1:cutoff),]
#don't need this anymore
#datTrain <- poplar_rand[-c(41:56),]
#datTest <- poplar_rand[41:56,]
str(datTrain)
str(datTest)

#Plot of Vcmax
resultV <- plsrPlot(Vcmax ~ NIR, data = datTrain, testdata = datTest,
                   ncomp = "auto", maxcomp = 10,
                   validation = "CV", segment.type ="interleaved",
                   output = FALSE, return.stats=TRUE)


resultJ <- plsrPlot(Jmax ~ NIR, data = datTrain, testdata = datTest,
                   ncomp = "auto", maxcomp = 10,
                   validation = "CV", segment.type ="interleaved",
                   output = FALSE, return.stats=TRUE)

round(resultV$R.test^2 ,3)
round(resultJ$R.test^2, 3)


R_squared_spread <- function(x, prop_train){
        x_rand <- x[sample(nrow(x)),]
        cutoff = round(prop_train*nrow(x_rand))
        datTrain <- x_rand[1:cutoff,]
        datTest <- x_rand[-(1:cutoff),]
        resultV <- plsrPlot(Vcmax ~ NIR, data = datTrain, testdata = datTest,
                            ncomp = "auto", maxcomp = 10,
                            validation = "CV", segment.type ="interleaved",
                            output = FALSE, return.stats=TRUE, plot=FALSE)
        return(round(resultV$R.test^2 ,3))
        
}


Repeat_R_squared <- function(x, n){
        pb <- progress_bar$new(total = 100)
        pb$tick()
        Sys.sleep(0.1)
        Prop_0.3 <- do.call(rbind, rlply(n, R_squared_spread(x,0.3), .progress = "text"))
        Prop_0.4 <- do.call(rbind, rlply(n, R_squared_spread(x,0.4), .progress = "text"))
        Prop_0.5 <- do.call(rbind, rlply(n, R_squared_spread(x,0.5), .progress = "text"))
        Prop_0.6 <- do.call(rbind, rlply(n, R_squared_spread(x,0.6), .progress = "text"))
        Prop_0.7 <- do.call(rbind, rlply(n, R_squared_spread(x,0.7), .progress = "text"))
        Prop_0.8 <- do.call(rbind, rlply(n, R_squared_spread(x,0.8), .progress = "text"))
        Prop_0.9 <- do.call(rbind, rlply(n, R_squared_spread(x,0.9), .progress = "text"))
        return(all_prop <- combine(Prop_0.3, Prop_0.4, Prop_0.5, Prop_0.6, Prop_0.7, Prop_0.8, Prop_0.9))
}

Repeat_R_squared(poplar,5)

Prop_0.3 <- do.call(rbind, rlply(100, R_squared_spread(poplar,0.3)))
Prop_0.4 <- do.call(rbind, rlply(100, R_squared_spread(poplar,0.4)))
Prop_0.5 <- do.call(rbind, rlply(100, R_squared_spread(poplar,0.5)))
Prop_0.6 <- do.call(rbind, rlply(100, R_squared_spread(poplar,0.6)))
Prop_0.7 <- do.call(rbind, rlply(100, R_squared_spread(poplar,0.7)))
Prop_0.8 <- do.call(rbind, rlply(100, R_squared_spread(poplar,0.8)))
Prop_0.9 <- do.call(rbind, rlply(100, R_squared_spread(poplar,0.9)))

all_prop <- combine(Prop_0.3, Prop_0.4, Prop_0.5, Prop_0.6, Prop_0.7, Prop_0.8, Prop_0.9)

ggplot(all_prop, aes(y=data, x=source)) + 
        geom_point(size=2)+
        theme_bw(base_size=14)+labs(title = "Variance around R-squared based on prop. training data", y="R-squared", x="Proportion Training Data (%)")

