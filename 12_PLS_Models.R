#Author: Mallory Barnes
#Date: 01/30/2017
#Input: 
#Output: 

library(pls)
library(devtools)
library(signal)
library(dplyr)
#install_github("uwadaira/plsropt", dependencies = TRUE)
library(plsropt)

#all the following from: plsropt/R/plsrauto.R

#' @title PLS regressions under different combinations of X-variable range and preprocessing method
#'
#' @description The function \code{plsrauto} performs the partial least squares (PLS) regressions under different combinations of X-variable range and preprocessing method automatically.
#'
#' @param formula	a model formula like \code{y ~ x}. See \code{\link{plsr}} and \code{\link{lm}} for a detailed description.
#' @param testdata data set for prediction.
#' @param xrange an object of class \code{list} which contains ranges of X-variables (see below).
#' @param p filter order for Savitzky-Golay smoothing (default value is 2).
#' @param n filter length (window size) for Savitzky-Golay smoothing (must be odd. default value is 11).
#' @param output if \code{TRUE}, the results are output as PDF and CSV files in the `PLSR_auto' directory.
#' @param ... additional arguments passed to \code{\link{plsrPlot}} and \code{\link{plsr}} in 'pls' package.
#'
#' @details Three steps of preprocessing are automatically applied to the X-variable data set.
#' First, standard normal variate (SNV) is applied or not.
#' Second, Savitzky-Golay smoothing, 1st derivative or 2nd derivative is applied or not, respectively.
#' Finally, auto-scaling is applied or not.
#' Total 16 (2*4*2) kinds of preprocessing methods are applied and the results of PLS regressions are returned as an object of class \code{data.frame}.
#' If \code{output == TRUE}, each PLS regression result is output as PDF and CSV files in one directory.
#'
#' @return an object of class \code{data.frame} containing the statistics of PLS regressions under the different combinations of X-variable range and preprocessing method is returned.
#'
#' @seealso \code{\link{plsrPlot}}, \code{\link{plsr}}, \code{\link{lm}}
#'
#' @examples
#' data(peach)
#' datTrain <- peach[1:50, ]
#' datTest  <- peach[51:74, ]
#' result.all <- plsrauto(Brix ~ NIR, data = datTrain, testdata = datTest, xrange = list(c(700, 1098), c(1100, 2498)))
#'
#' @import signal
#' @export

summary(peach)
str(peach)
head(peach)
data(peach)
matplot(colnames(peach$NIR), t(peach$NIR), type = "l", lty = 1,
        xlab = "wavelength (nm)", ylab = "log(1/R)",
        main = "NIR spectra of intact peach fruits")

datAll <- read.csv(file = "C:/Users/rsstudent/Dropbox/Drought_Expt_2016/peachNIR.csv", row.names = 1, check.names = F)
str(datAll)
rownames(datAll)
colnames(datAll)[1:5]
colnames(datAll)[(NCOL(datAll)-4):NCOL(datAll)]
x <- extdat(datAll, start = 700, end = 2498)
peach <- data.frame(datAll[, 1:2], NIR = I(as.matrix(x)))

peach$NIR <- extdat(peach$NIR, start = 700, end = 1098)
peach$NIR <- snv(peach$NIR)
peach$NIR <- matsgolay(peach$NIR, p=2, n=11, m=2)
peach$NIR <- scale(peach$NIR, center = TRUE, scale = TRUE)

datTrain <- peach[1:50, ]
datTest <- peach[51:74, ]


result <- plsrPlot(Brix ~ NIR, data = datTrain, testdata = datTest,
                   ncomp = "auto", maxcomp = 10,
                   validation = "CV", segment.type ="interleaved",
                   output = FALSE)

#Ok now for my own data!!!!!!!!!!!!!

poplar_names <- read.csv("C:/Users/Mallory/Dropbox/Drought_Expt_2016/poplar_allwavelengths.csv")
str(poplar_names)
poplar_names$date.y <- as.Date(poplar_names$date.y)
#When subsetting by genotype: 
poplar_names <- subset(poplar_names, Genotype=="R-270")

#When ordering by water potential (descending)
#poplar_names <- poplar_names[order(-poplar_names$Water_Pot),] 

#When ordering by date (descending)
poplar_names <- poplar_names[order(poplar_names$date.y),] 
poplar_names$date.y

#get rid of 'x' column, Water_Pot, and Genotype for now 
poplar_names <- poplar_names[ -c(1, 6:7)]
#change col names (all say 'x' right now(?))
names(poplar_names) <- gsub("X", "", names(poplar_names))
str(poplar_names)
#rownames of all data
poplar_all <- poplar_names[,-1]
rownames(poplar_all) <- poplar_names[,1]
rownames(poplar_all)
#check column names
colnames(poplar_all)[1:5]
#last five column names
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

datTrain <- poplar[-c(1:18),]
datTest <- poplar[19:28,]


#Shuffle Rowwise: 
#poplar_rand <- poplar[sample(nrow(poplar)),]

str(datTrain)
str(datTest)

#This is for only obs on June 23rd and 24th
#datTrain <- poplar[-c(25:33),]
#datTest <- poplar[25:33,]

#This is for only obs on June 23rd and 24th for GT 52-276
datTrain <- poplar[-c(13:17),]
datTest <- poplar[13:17,]

result <- plsrPlot(Vcmax ~ NIR, data = datTrain, testdata = datTest,
                   ncomp = "auto", maxcomp = 10,
                   validation = "CV", segment.type ="interleaved",
                   output = FALSE)
result$validation
result.all <- plsrauto(Vcmax ~ NIR, data = datTrain, testdata = datTest, xrange = list(c(350, 2500)))
result.all$R.val


