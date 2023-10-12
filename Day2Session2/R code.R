## ---- eval=FALSE, echo=FALSE, purl=TRUE---------------------------------------
## ## Install packages automatically if they are not available on your computer
## for (i in c("metaSEM", "symSEM", "lavaan", "semPlot", "readxl")) {
##   if (!(i %in% rownames(installed.packages()))) install.packages(i)
## }


## ---- message=FALSE-----------------------------------------------------------
library(metaSEM)

## Sample correlation matrix
R <- matrix(c(1, .3, .4,
              .3, 1, .5,
              .4, .5, 1), nrow=3, ncol=3,
            dimnames = list(c("x1", "x2", "x3"), c("x1", "x2", "x3")))
R
vechs(R)

## Compute the sampling covariance matrix of the correlations with n=100
asyCov(R, n=100)




## ---- message=FALSE-----------------------------------------------------------
## Load the library for MASEM
library(metaSEM)

## Load the library to read XLSX file
library(readxl)

## Read the study characteristics
study <- read_xlsx("Digman97.xlsx", sheet="Info")

## Display a few studies
head(study)


## -----------------------------------------------------------------------------
## Create an empty list to store the correlation matrices
Digman97.data <- list()
  
## Read Study 1 to 14 correlation matrices
for (i in 1:14) {
  ## Read each sheet and convert it into a matrix
  mat <- as.matrix(read_xlsx("Digman97.xlsx", sheet=paste0("Study ", i)))
  ## Add the row names
  rownames(mat) <- colnames(mat)
  ## Save it into a list
  Digman97.data[[i]] <- mat
}

## Add the names of the studies
names(Digman97.data) <- study$Study

## Show the first few studies
head(Digman97.data)


## -----------------------------------------------------------------------------
## As we read the full correlation matrices from Excel, we want to ensure that the matrices are symmetric and positive definite.
is.pd(Digman97.data)

## Extract the sample sizes
Digman97.n <- study$n
Digman97.n

## Extract the cluster for subgroup analysis
Digman97.cluster <- study$Cluster
Digman97.cluster


## -----------------------------------------------------------------------------
## Display the no. of studies
pattern.na(Digman97.data, show.na=FALSE)

## Display the cumulative sample sizes
pattern.n(Digman97.data, Digman97.n)


## -----------------------------------------------------------------------------
## Specify the model with default labels
model1 <- "## Factor loadings
           ## Alpha is measured by A, C, and ES
           Alpha =~ A + C + ES
           ## Beta is measured by E and I
           Beta =~ E + I
           ## Factor correlation between Alpha and Beta
           Alpha ~~ Beta"

## Display the model
## See ?plot.character for the help page
plot(model1, color="yellow")


## -----------------------------------------------------------------------------
## Defaults: A.notation="ON", S.notation="WITH"
RAM1 <- lavaan2RAM(model1, obs.variables=c("A","C","ES","E","I"), A.notation="on", S.notation="with")
RAM1


## -----------------------------------------------------------------------------
## Specify the model with our own labels
model2 <- "## Alpha is measured by A, C, and ES
           Alpha =~ a*A + b*C + c*ES
           ## Beta is measured by E and I
           Beta =~ d*E + e*I
           ## Factor correlation between Alpha and Beta
           Alpha ~~ f*Beta"

## Display the model
plot(model2, color="cyan")

RAM2 <- lavaan2RAM(model2, obs.variables=c("A","C","ES","E","I"))
RAM2




## -----------------------------------------------------------------------------
## method="FEM": fixed-effects TSSEM
fixed1 <- tssem1(Digman97.data, Digman97.n, method="FEM")

## summary of the findings
summary(fixed1)

## extract coefficients
coef(fixed1)


## -----------------------------------------------------------------------------
fixed2 <- tssem2(fixed1, RAM=RAM1)
summary(fixed2)

plot(fixed2, color="green")


## -----------------------------------------------------------------------------
# Display the original study characteristic
table(Digman97.cluster)     

## Younger participants: "Children" and "Adolescents"
## Older participants: "Mature adults"
sample <- ifelse(Digman97.cluster %in% c("Children", "Adolescents"), 
                 yes="Younger participants", no="Older participants")
table(sample)


## -----------------------------------------------------------------------------
## cluster: variable for the analysis with cluster
fixed1.cluster <- tssem1(Digman97.data, Digman97.n, method="FEM", cluster=sample)

summary(fixed1.cluster)


## -----------------------------------------------------------------------------
fixed2.cluster <- tssem2(fixed1.cluster, RAM=RAM1)
summary(fixed2.cluster)


## -----------------------------------------------------------------------------
## Setup two plots side-by-side
layout(t(1:2))

## Plot the first group
plot(fixed2.cluster[[1]], col="green")
title("Younger participants")

## Plot the second group
plot(fixed2.cluster[[2]], col="yellow")
title("Older participants")


## ---- warning=TRUE------------------------------------------------------------
## Let's try to estimate all elements in the variance component matrix RE.type="Symm".
## Please note that this model is problematic as there are too many parameters.
random0 <- tssem1(Digman97.data, Digman97.n, method="REM", RE.type="Symm")
summary(random0)




## -----------------------------------------------------------------------------
## method="REM": Random-effects model
random1 <- tssem1(Digman97.data, Digman97.n, method="REM", RE.type="Diag")
summary(random1)

## Extract the fixed-effects estimates
(est_fixed <- coef(random1, select="fixed"))

## It is difficult to read the above estimates.
## We may convert the estimated vector to a symmetrical matrix
## where the diagonals are fixed at 1 (for a correlation matrix)
averageR <- vec2symMat(est_fixed, diag=FALSE)
dimnames(averageR) <- list(c("A", "C", "ES", "E", "I"),
                           c("A", "C", "ES", "E", "I"))
averageR

## We may get the variance component matrix by
VarCorr(random1)




## ---- warning=FALSE-----------------------------------------------------------
random2 <- tssem2(random1, RAM=RAM1)
summary(random2)

## Plot the parameter estimates
plot(random2, color="green")

