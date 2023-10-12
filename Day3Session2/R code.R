## ---- eval=FALSE, echo=FALSE, purl=TRUE---------------------------------------
## ## Install packages automatically if they are not available on your computer
## for (i in c("metaSEM", "symSEM", "lavaan", "semPlot", "readxl")) {
##   if (!(i %in% rownames(installed.packages()))) install.packages(i)
## }




## ---- message=FALSE-----------------------------------------------------------
library(metaSEM)

## Load the library to read the XLSX file
library(readxl)

## Read the study characteristics
my.df <- read_xlsx("Schutte21.xlsx", sheet="Sheet 1")

head(my.df)

table(my.df$Type_of_Association)


## -----------------------------------------------------------------------------
# |             | Mindfulness | EI       | Gratitude |
# |-------------+-------------+----------+-----------|
# | Mindfulness | 1.0         |          |           |
# | EI          | mat[2,1]    | 1.0      |           |
# | Gratitude   | mat[3,1]    | mat[3,2] |       1.0 |

create.matrix <- function(x, type=c("mindfulness and emotional intelligence", 
                                    "mindfulness and gratitude", 
                                    "emotional intelligence and gratitude")) {
  mat <- matrix(NA, ncol=3, nrow=3)
  diag(mat) <- 1
  type <- match(type, 
                c("mindfulness and emotional intelligence", 
                  "mindfulness and gratitude", 
                  "emotional intelligence and gratitude"))
  switch(type,
         "mindfulness and emotional intelligence" = mat[1, 2] <- mat[2, 1] <- unlist(x),
         "mindfulness and gratitude" = mat[1, 3] <- mat[3, 1] <- unlist(x),
         "emotional intelligence and gratitude" = mat[2, 3] <- mat[3, 2] <- unlist(x))
  mat
}

my.vars <- c("Mindfulness", "EI", "Gratitude")

my.cor <- lapply(split(my.df, seq(nrow(my.df))),
                 function(x) create.matrix(x["Effect_size"], x["Type_of_Association"]))

## Add the dimnames for reference
my.cor <- lapply(my.cor, function(x) {dimnames(x) <- list(my.vars, my.vars); x}  )

## Add the study names for reference
names(my.cor) <- my.df$Study_name

## Correlation matrices in the analysis
head(my.cor)


## -----------------------------------------------------------------------------
## Sample sizes
my.n <- my.df$N
hist(my.n)

## Number of studies in each cell
pattern.na(my.cor, show.na = FALSE)

## Total sample sizes in each cell
pattern.n(my.cor, my.n)


## -----------------------------------------------------------------------------
## Stage 1 analysis: find an average correlation matrix
stage1 <- tssem1(my.cor, my.n, method="REM")
summary(stage1)


## -----------------------------------------------------------------------------
meanR <- vec2symMat(coef(stage1, select = "fixed"), diag = FALSE)
dimnames(meanR) <- list(my.vars, my.vars)
meanR


## -----------------------------------------------------------------------------
tau2 <- vec2symMat(coef(stage1, select = "random"), diag = FALSE)
dimnames(tau2) <- list(my.vars, my.vars)
tau2


## -----------------------------------------------------------------------------
I2 <- vec2symMat(summary(stage1)$I2.values[, "Estimate"], diag = FALSE)
dimnames(I2) <- list(my.vars, my.vars)
I2


## ---- message=FALSE-----------------------------------------------------------
## Proposed model
model1 <- "Gratitude ~ c*Mindfulness + b*EI
           EI ~ a*Mindfulness
           Mindfulness ~~ 1*Mindfulness
           ## Define direct and indirect effects
           Direct := c
           Indirect := a*b"

plot(model1, color="yellow")

RAM1<- lavaan2RAM(model1, obs.variables = my.vars)
RAM1


## -----------------------------------------------------------------------------
stage2 <- tssem2(stage1, RAM=RAM1, intervals.type = "LB")
summary(stage2)

plot(stage2, color="yellow")


## ---- message=FALSE-----------------------------------------------------------
## Model with direct effect equals indirect effect
model2 <- "Gratitude ~ c*Mindfulness + b*EI
           EI ~ a*Mindfulness
           Mindfulness ~~ 1*Mindfulness
           ## Define direct and indirect effects
           Direct := c
           Indirect := a*b
           ## Add a constraint to ensure c = a*b
           c == a*b        
"

RAM2 <- lavaan2RAM(model2, obs.variables = my.vars)
RAM2


## -----------------------------------------------------------------------------
stage2b <- tssem2(stage1, RAM=RAM2, intervals.type = "LB")
summary(stage2)

plot(stage2b, color="yellow")


## -----------------------------------------------------------------------------
mxEval(c-a*b, stage2b$mx.fit)

## Compare the models with and without c=a*b
## df=1 because we are comparing 1 constraint.
anova(stage2, stage2b)


## -----------------------------------------------------------------------------
model <- "Beh ~ b*Int + c*PB
          Int ~ a*PB
          PB ~~ 1*PB"
plot(model, col="yellow")


## -----------------------------------------------------------------------------
## Load the dataset
load("Hagger18.Rdata")

head(Hagger18)


## -----------------------------------------------------------------------------
## The variables are arranged as the outcome, mediator, and predictor: Beh, Int, and PB.
Indirect.eff <- indirectEffect(Hagger18$data, Hagger18$n)

## Add behavior frequency to the data
Indirect.eff <- data.frame(Indirect.eff, beh_freq_high=Hagger18$beh_freq_high)

## Show the first few studies
head(Indirect.eff)


## -----------------------------------------------------------------------------
## Random-effects model
IE0 <- meta(y=cbind(ind_eff, dir_eff),
            v=cbind(ind_var, ind_dir_cov, dir_var),
            data=Indirect.eff,
            model.name = "Random")
summary(IE0)


## -----------------------------------------------------------------------------
VarCorr(IE0)

## Correlation matrix of the random effects: small correlation
cov2cor(VarCorr(IE0))


## -----------------------------------------------------------------------------
plot(IE0, axis.labels = c("Indirect effect", "Direct effect"))


## -----------------------------------------------------------------------------
IE1 <- meta(y=cbind(ind_eff, dir_eff),
            v=cbind(ind_var, ind_dir_cov, dir_var),
            x=beh_freq_high,
            data=Indirect.eff,
            model.name = "Moderator_beh")
summary(IE1)

## Test the statistical significance between the models
anova(IE1, IE0)


## -----------------------------------------------------------------------------
library(metaSEM)

## Show how many cores are in my computer
parallel::detectCores()

## Use all cores-2, i.e., keep 2 cores for other operations.
mxOption(key='Number of Threads', value=parallel::detectCores()-2)


## -----------------------------------------------------------------------------
## x is a string vector
x <- c(10.1, 23.4, "2.1 ", " 3.4", "\t4.2", "3.1\n")
x
is.numeric(x)
is.character(x)

## Convert it to numeric
y <- as.numeric(x)
y
is.numeric(y)


## -----------------------------------------------------------------------------
## X1 is okay
X1 <- matrix(c(1, 0.8, 0.8, 1), nrow=2)
X1
is.pd(X1)

## X2 is not okay, as the correlation is 1.2.
X2 <- matrix(c(1, 1.2, 1.2, 1), nrow=2)
X2
is.pd(X2)

## X3 is not okay, as the correlation matrix is not symmetric.
X3 <- matrix(c(1, 0.6, 0.8, 1), nrow=2)
X3
isSymmetric(X3)






## -----------------------------------------------------------------------------
pattern.na(Hunter83$data, show.na=FALSE)


## -----------------------------------------------------------------------------
## Sample data from Digman (1997)
Digman97$data[1:2]

# TSSEM
## Stage 1 TSSEM with a random-effects model
rand1 <- tssem1(Digman97$data, Digman97$n, method="REM")
summary(rand1)


## -----------------------------------------------------------------------------
model <- "## One-factor model with 3 indicators
          Alpha=~A+C+ES"
plot(model, col="yellow")  

RAM <- lavaan2RAM(model, obs.variables=c("A","C","ES"))
RAM


## -----------------------------------------------------------------------------
## Use subset.variables to include variables in the analysis
rand2 <- tssem2(rand1, RAM=RAM, subset.variables=c("A","C","ES"))
summary(rand2)

## Display the model with the parameter estimates
plot(rand2, color="green")


## -----------------------------------------------------------------------------
## Convert the data to the right format
df <- Cor2DataFrame(Digman97)

## Use subset.variables to include variables in the analysis
osmasem.fit <- osmasem(RAM=RAM, data=df, subset.variables=c("A","C","ES"))
summary(osmasem.fit)

plot(osmasem.fit, color="green")

