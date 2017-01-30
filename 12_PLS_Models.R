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
