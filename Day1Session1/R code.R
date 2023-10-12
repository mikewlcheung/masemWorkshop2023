## -----------------------------------------------------------------------------
## Install packages automatically if they are not available on your computer
for (i in c("metaSEM", "symSEM", "lavaan", "semPlot", "readxl")) {
  if (!(i %in% rownames(installed.packages()))) install.packages(i)
}


































## ---- message=FALSE-----------------------------------------------------------
## Library to fit SEM
library(lavaan)

## Sample covariance matrix in the lower triangle
lower <- '
11.834          
6.947, 9.364        
6.819, 5.091, 12.532      
4.783, 5.028, 7.495, 9.986    
-3.839, -3.889, -3.841, -3.625, 9.610  
-2.190, -1.883, -2.175, -1.878, 3.552, 4.503'

## Convert a lower triangle data into a covariance matrix
df <- getCov(lower, diag=TRUE, names=c("anomia67", "power67", "anomia71", "power71", "educ", "SEI"))
df


## -----------------------------------------------------------------------------
my.model1 <- 'AL67 =~ anomia67 + power67    # AL67 is measured by anomia67 and power67; latent variable =~ observed variables
              AL71 =~ anomia71 + power71    # AL71 is measured by anomia71 and power71
              SES =~ educ + SEI             # SES is measured by educ and SEI
              AL71 ~ SES + AL67             # AL71 is modeled by SES and AL67
              AL67 ~ SES                    # AL67 is modeled by SES
'

## Use a covariance matrix and the sample size as the inputs
my.fit1 <- sem(my.model1, sample.cov=df, sample.nobs=932)

## Get the fit measures and standardized solution
summary(my.fit1, fit.measures=TRUE, standardized=TRUE) 


## -----------------------------------------------------------------------------
## Library to plot SEM
library(semPlot)

# ## Plot the unstandardized solution
semPaths(my.fit1, whatLabels="est", color="green")

## Plot the standardized solution
semPaths(my.fit1, whatLabels="stand", color="yellow")




## ---- message=FALSE-----------------------------------------------------------
my.model2 <- 'AL67 =~ anomia67 + power67    # AL67 is measured by anomia67 and power67
              AL71 =~ anomia71 + power71    # AL71 is measured by anomia71 and power71
              SES =~ educ + SEI             # SES is measured by educ and SEI
              AL71 ~ SES + AL67             # AL71 is modeled by SES and AL67
              AL67 ~ SES                    # AL67 is modeled by SES
              anomia67 ~~ anomia71          # Correlated residuals between anomia67 and anomia71 
              power67 ~~ power71            # Correlated residuals between power67 and power71
'

my.fit2 <- sem(my.model2, sample.cov=df, sample.nobs=932)

## Get the fit measures and standardized solutions
summary(my.fit2, fit.measures=TRUE, standardized=TRUE)


## -----------------------------------------------------------------------------
## Plot the standardized solution
semPaths(my.fit2, whatLabels="stand", color="green")

## Compare these two nested models
anova(my.fit1, my.fit2)

