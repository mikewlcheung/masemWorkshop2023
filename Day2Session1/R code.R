## ---- eval=FALSE, echo=FALSE, purl=TRUE---------------------------------------
## ## Install packages automatically if they are not available on your computer
## for (i in c("metaSEM", "symSEM", "lavaan", "semPlot", "readxl")) {
##   if (!(i %in% rownames(installed.packages()))) install.packages(i)
## }




## ---- message=FALSE-----------------------------------------------------------
## Load the libraries
library(metaSEM)
library(symSEM)

## Specify the model
cfa <- "f =~ a*x1 + b*x2 + c*x3
        f ~~ 1*f            ## Fix the factor variance at 1 for identification
        x1 ~~ e11*x1        ## Label the error variances
        x2 ~~ e22*x2
        x3 ~~ e33*x3"

## Plot the model
plot(cfa, color="yellow")

## Convert it to RAM specification
## We will introduce the RAM specification in later.
RAM <- lavaan2RAM(cfa, obs.variables=c("x1", "x2", "x3"))

## Print the model-implied covariance matrix
impliedS(RAM, corr=FALSE)

## Print the model-implied correlation matrix
impliedS(RAM, corr=TRUE)

