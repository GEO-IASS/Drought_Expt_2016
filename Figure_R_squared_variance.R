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


#Shuffle Rowwise: 
str(poplar)
poplar_rand <- poplar[sample(nrow(poplar)),]
#Break into training and testing set 
#Doesn't let plsrplot run properly
cutoff = round(0.7*nrow(poplar_rand))

datTrain <- poplar_rand[1:cutoff,]
datTrain <- poplar_rand[-(1:cutoff),]

datTrain <- (sample_frac(poplar, size = .20, replace = TRUE))
datTest <- setdiff(poplar_rand, datTrain)
#plsrplot runs properly
datTrain <- poplar_rand[-c(41:56),]
datTest <- poplar_rand[41:56,]

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

result

resultV$validation
result.all <- plsrauto(Vcmax ~ NIR, data = datTrain, testdata = datTest, xrange = list(c(350, 2500)))
result.all$R.val

cor(y.test, yhat.test)^2

yhat.test <- predict(resultV, ncomp = ncomp, newdata = as.matrix(x.test))

result.test.lm <- lm(yhat.test ~ y.test)
stats <- data.frame(stats, N.test=length(y.test), R.test=cor(y.test, yhat.test), Slope.test, SEP, Bias.test, RPD.test)
