#Author: Mallory Barnes
#Date: 01/30/2017
#Input: 
#Output: 

library(pls)
library(devtools)
library(signal)
install_github("uwadaira/plsropt", dependencies = TRUE)
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


#Sample dataset: Peach
#NIR spectra contains the absorbance values in the wavelength range of 700 - 2498 nm (2 nm)   
# load a sample data
data(peach)
str(peach)

# plot the spectra
head(peach)
matplot(colnames(peach$NIR), t(peach$NIR), type = "l", lty = 1,
        xlab = "wavelength (nm)", ylab = "log(1/R)",
        main = "NIR spectra of intact peach fruits")

#CSV containing the sample name, the objective variables (Y-variables) and the 
#explanatory variables (X-variables, ex. spectral data) in this order
datAll <- read.csv(file = "C:/Users/Mallory/Dropbox/Drought_Expt_2016/peachNIR.csv", row.names = 1, check.names = F)

# row name of the data set
# first five column names
colnames(datAll)[1:5]
# last five column names
colnames(datAll)[(ncol(datAll)-4):ncol(datAll)]
# extract X-variables
x <- extdat(datAll, start = 700, end = 2498)
# reintegrate Y-variables with X-variables
peach <- data.frame(datAll[, 1:2], NIR = I(as.matrix(x)))
# extract the range of 700 nm - 1098 nm
peach$NIR <- extdat(peach$NIR, start = 700, end = 1098)
# Standard normal variate (SNV)
peach$NIR <- snv(peach$NIR)

# Savitzky-Golay second derivative
#1st derivative or 2nd derivative to the data set
#by using the argument m = 0 , m = 1 or m = 2
# p is the filter order and n is window size
peach$NIR <- matsgolay(peach$NIR, p=2, n=11, m=2)

# Auto-scaling
peach$NIR <- scale(peach$NIR, center = TRUE, scale = TRUE)

#Divide data set into training and test set
datTrain <- peach[1:50, ]
datTest <- peach[51:74, ]

#plsrPlot draws four kinds of plots which are useful for understanding the model.
result <- plsrPlot(Brix ~ NIR, data = datTrain, testdata = datTest,
                   ncomp = "auto", maxcomp = 10,
                   validation = "CV", segment.type ="interleaved",
                   output = FALSE)

#Find the optimal model
# reload dat set
data(peach)

# X-variable ranges to be compared
xrange <- list(c(700, 1098), c(1100, 2498))

#Divide the data set into training and test data set - skip if want to use all samples as training
datTrain <- peach[1:50, ]
datTest <- peach[51:74, ]

#function plsrauto . If you want to use all samples as training set, set data = dat and
#delete testdata = datTest
result.all <- plsrauto(Brix ~ NIR, data = datTrain,
                       testdata = datTest, xrange = xrange,
                       validation = "CV", segment.type ="interleaved")

#The number of segments is changed by the argument of seguments (default value is 10).
#The number of (consecutive) rows that are replicates of the same object is set by nrep 
#Replicates will always be kept in the same segment for preventing overfittng.
#result.all is sorted by the correlation coefficient of validation set R.val (or R.test if test
                                                                            # set is used)
#combination of X-variable range 1100-2498 and prerocessing 
#method SNV + SG2D(wsize-11pt_forder-2) + Auto-scaling shows the highest R.test .
result.all[1:5,]

# save the result as a CSV file
write.csv(result.all, file = "plsrauto_result.csv")

