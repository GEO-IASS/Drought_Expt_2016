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


#Need to get data in proper format: 


