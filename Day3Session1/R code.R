## ---- eval=FALSE, echo=FALSE, purl=TRUE---------------------------------------
## ## Install packages automatically if they are not available on your computer
## for (i in c("metaSEM", "symSEM", "lavaan", "semPlot", "readxl")) {
##   if (!(i %in% rownames(installed.packages()))) install.packages(i)
## }


## ---- warning=TRUE, message=FALSE---------------------------------------------
library(metaSEM)

## Stage 1 analysis
random1 <- tssem1(Digman97$data, Digman97$n, method="REM", RE.type="Diag")

## Model with correlated factors
model1 <- "## Alpha is measured by A, C, and ES
           Alpha =~ A + C + ES
           ## Beta is measured by E and I
           Beta =~ E + I
           ## Factor correlation between Alpha and Beta
           Alpha ~~ Beta"

RAM1 <- lavaan2RAM(model1, obs.variables=c("A","C","ES","E","I"))
RAM1

## Display the model
plot(model1, color="yellow")


## -----------------------------------------------------------------------------
random2 <- tssem2(random1, RAM=RAM1)
summary(random2)


## ---- warning=TRUE, message=TRUE----------------------------------------------
## Model with uncorrelated factors
model2 <- "## Alpha is measured by A, C, and ES
           Alpha =~ A + C + ES
           ## Beta is measured by E and I
           Beta =~ E + I
           ## Factor correlation between Alpha and Beta fixed at 0
           Alpha ~~ 0*Beta"

## Display the model
plot(model2, color="yellow")

RAM2 <- lavaan2RAM(model2, obs.variables=c("A","C","ES","E","I"))
RAM2


## -----------------------------------------------------------------------------
random2a <- tssem2(random1, RAM=RAM2)
summary(random2a)

## OpenMx statu1 = 5, we may rerun the analysis to see if we can get better results
random2a <- rerun(random2a)
summary(random2a)


## -----------------------------------------------------------------------------
## Plot the parameter estimates
plot(random2a, color="green")

## Comparing them with a chi-square statistic
anova(random2, random2a)


## ---- warning=TRUE------------------------------------------------------------
## Model with a general personality factor
model3 <- "## Factor loadings
           G =~ A + C + ES + E + I"

## Display the model
plot(model3, color="yellow")

RAM3 <- lavaan2RAM(model3, obs.variables=c("A","C","ES","E","I"),
                   A.notation = "on", S.notation = "with")
RAM3


## -----------------------------------------------------------------------------
random2b <- tssem2(random1, RAM=RAM3)
summary(random2b)


## -----------------------------------------------------------------------------
## Plot the parameter estimates
plot(random2b, color="green")

## Comparing them with a chi-square statistic
anova(random2, random2b)


## -----------------------------------------------------------------------------
## Model with the same factor loadings within the factor
model4 <- "## Factor loadings
           ## Alpha is measured by A, C, and ES
           Alpha =~ alpha*A + alpha*C + alpha*ES
           ## Beta is measured by E and I
           Beta =~ beta*E + beta*I
           ## Factor correlation between Alpha and Beta
           Alpha ~~ Beta"

## Display the model
plot(model4, color="yellow")

RAM4 <- lavaan2RAM(model4, obs.variables=c("A","C","ES","E","I"))
RAM4


## -----------------------------------------------------------------------------
random2c <- tssem2(random1, RAM=RAM4)
summary(random2c)

## Plot the parameter estimates
plot(random2c, color="green")

## Comparing them with a chi-square statistic
anova(random2, random2c)


## ---- message=FALSE-----------------------------------------------------------
library(readxl)
library(metaSEM)

## Read the Excel file
df <- read_xlsx("Nohe15.xlsx", sheet="Table A1")

head(df)


## -----------------------------------------------------------------------------
## Variable names
my.var <- c("W1", "S1", "W2", "S2")

## Split each row as a list
my.list <- split(df, 1:nrow(df))


## Select the correlation coefficients and convert it into a correlation matrix
my.cor <- lapply(my.list, 
                 function(x) {## Convert the correlations into a correlation matrix
                              mat <- vec2symMat(unlist(x[c("W1-S1","W1-W2","W1-S2","S1-W2","S1-S2","W2-S2")]), diag = FALSE)
                              ## Add the dimensions for ease of reference
                              dimnames(mat) <- list(my.var, my.var)
                              ## Return the correlation matrix
                              mat})

## Add the study names for ease of reference
names(my.cor) <- df$Study

head(my.cor)


## -----------------------------------------------------------------------------
## Sample sizes
hist(df$N, breaks=10)

## Display the no. of studies
pattern.na(my.cor, show.na=FALSE)

## Display the cumulative sample sizes
pattern.n(my.cor, df$N)


## ---- message=FALSE-----------------------------------------------------------
## Fit the stage one model (fixed-effects model)
fix1 <- tssem1(my.cor, df$N, method="FEM")
summary(fix1)


## -----------------------------------------------------------------------------
## Fit the stage one model (random-effects model)
rand1 <- tssem1(my.cor, df$N, method="REM")
summary(rand1)


## -----------------------------------------------------------------------------
## Extract the fixed-effects estimates
R1 <- coef(rand1, select = "fixed")
R1

## Convert it to a correlation matrix
R1 <- vec2symMat(R1, diag = FALSE)
R1

## Add the dimension names for ease of reference
dimnames(R1) <- list(my.var, my.var)
R1

## Heterogeneity: variance-covariance component of the random effects
VarCorr(rand1)


## -----------------------------------------------------------------------------
## Model in lavaan syntax
## The parameter labels are not necessary. But they make the output easier to follow.
model1 <- "W2 ~ w2w*W1 + s2w*S1    # path coefficients   
           S2 ~ s2s*S1 + w2s*W1
           S1 ~~ w1withs1*W1       # correlation
           S2 ~~ w2withs2*W2
           W1 ~~ 1*W1              # variances of the independent variables are fixed at 1
           S1 ~~ 1*S1"

## Display the model
## See the layout argument in ?semPaths
## layout can be either tree, circle, spring, tree2, or circle2
plot(model1, color="yellow", layout="spring")

## Convert the above model to the RAM specification used by metaSEM.
## We also have to specify how the variables are arranged in the data.
RAM1 <- lavaan2RAM(model1, obs.variables = my.var)
RAM1


## -----------------------------------------------------------------------------
## Fit the stage two model
rand2a <- tssem2(rand1, RAM=RAM1)
summary(rand2a)

plot(rand2a, color="green", layout="spring")


## -----------------------------------------------------------------------------
model2 <- "W2 ~ same*W1 + cross*S1     # regression coefficients   
           S2 ~ same*S1 + cross*W1
           S1 ~~ w1cs1*W1             # correlation
           S2 ~~ w2cs2*W2
           W1 ~~ 1*W1                 # variances of the independent variables are fixed at 1
           S1 ~~ 1*S1"

## Display the model
plot(model2, color="yellow")

## Convert the above model to the RAM specification used by metaSEM.
## We also have to specify how the variables are arranged in the data.
RAM2 <- lavaan2RAM(model2, obs.variables = my.var)
RAM2


## -----------------------------------------------------------------------------
## Fit the stage two model
rand2b <- tssem2(rand1, RAM=RAM2)
summary(rand2b)

plot(rand2b, color="green")

## Comparing nested models with anova()
anova(rand2a, rand2b)


## -----------------------------------------------------------------------------
model3 <- "W2 ~ w2w*W1 + s2w*S1    # path coefficients   
           S2 ~ s2s*S1 + w2s*W1
           S1 ~~ w1withs1*W1       # correlation
           S2 ~~ w2withs2*W2
           W1 ~~ 1*W1              # variances of the independent variables are fixed at 1
           S1 ~~ 1*S1
           W2 ~~ VarW2*W2          # Label the error variances VarW2 and VarS2
           S2 ~~ VarS2*S2
           R2_W2 := 1 - VarW2      # Define R2 for W2
           R2_S2 := 1 - VarS2      # Define R2 for S2
"

RAM3 <- lavaan2RAM(model3, obs.variables = my.var)
RAM3


## -----------------------------------------------------------------------------
## Fit the stage two model
## diag.constraints=TRUE to involve the error variances in the model
rand2c <- tssem2(rand1, RAM=RAM3, diag.constraints=TRUE)
summary(rand2c)


## -----------------------------------------------------------------------------
## diag.constraints=TRUE to involve the error variances in the model
## intervals.type="LB" to get the LBCI on the R2
rand2d <- tssem2(rand1, RAM=RAM3, diag.constraints=TRUE, intervals.type="LB")
summary(rand2d)


## -----------------------------------------------------------------------------
## Convert the correlation matrices into a dataframe, which is required in osmasem()
Nohe.df <- Cor2DataFrame(x=my.cor, n=df$N)

## Display the frequency table of publication status
table(df$`Publication Status`)

## Create a dummy variable for publication
Pub <- ifelse(df$`Publication Status`=="P", yes=1, no=0)
table(Pub)

## Add Pub to the dataset
Nohe.df$data$Pub <- Pub

## Let's see how the dataset looks like
head(Nohe.df$data)


## -----------------------------------------------------------------------------
fit0 <- osmasem(model.name="No moderator", RAM=RAM1, data=Nohe.df)
summary(fit0)
plot(fit0, col="cyan")


## -----------------------------------------------------------------------------
## Create a matrix to present the "Pub" moderator
## We use "Pub" to moderate the following paths:
## W1 -> W2
## W1 -> S2
## S1 -> W2
## S1 -> S2
A1 <- create.modMatrix(RAM=RAM1, output="A", mod="Pub")
A1

## If we do not need to model all path coefficients, we may set them as "0".
## For example, we only want to model the path from W1 to W2.

## Create a copy of A1 for illustration
temp <- A1
temp[4, c(1, 2)] <- temp[3, 2] <- "0"
temp

## By default, we use the moderator to predict the path coefficients.
## If there are research questions to predict the correlations, we may try
S1 <- create.modMatrix(RAM=RAM1, output="S", mod="Pub")
S1


## -----------------------------------------------------------------------------
## Fit the model with Lag as a moderator: use Ax=A1 to specify A1 as the moderator 
fit1 <- osmasem(model.name="Pub as a moderator", RAM=RAM1, Ax=A1, data=Nohe.df)
summary(fit1)


## -----------------------------------------------------------------------------
## Comparing the models with and without Pub
## dfs = 4 as we are testing 4 regression coefficients
anova(fit1, fit0)

## beta1: coefficients of (published - unpublished)
mxEval(A1, fit1$mx.fit)

## After comparing the output, only w2s and s2w (rows 8 and 9) are statistically significant.

#        name  matrix row col    Estimate  Std.Error A     z value     Pr(>|z|)
# 1       w2w      A0  W2  W1  0.54341302 0.03389313    16.0331332 0.000000e+00
# 2       w2s      A0  S2  W1  0.14617724 0.03254016     4.4922107 7.048762e-06
# 3       s2w      A0  W2  S1  0.13941113 0.03349102     4.1626426 3.145856e-05
# 4       s2s      A0  S2  S1  0.56490746 0.03155965    17.8996773 0.000000e+00
# 5  w1withs1      S0  S1  W1  0.38053285 0.02324405    16.3711921 0.000000e+00
# 6  w2withs2      S0  S2  W2  0.17000538 0.02310466     7.3580566 1.865175e-13
# 7     w2w_1      A1  W2  W1  0.05019155 0.04408051     1.1386336 2.548560e-01
# 8     w2s_1      A1  S2  W1 -0.11152071 0.03879706    -2.8744631 4.047151e-03
# 9     s2w_1      A1  W2  S1 -0.09111004 0.04019669    -2.2666055 2.341434e-02
# 10    s2s_1      A1  S2  S1  0.03645812 0.04109654     0.8871335 3.750070e-01

## beta0: coefficients of the unpublished studies
mxEval(A0, fit1$mx.fit)

## beta0 + beta1: coefficients of the published studies
mxEval(A0+A1, fit1$mx.fit)


## -----------------------------------------------------------------------------
hist(df$Lag, breaks=15, xlab="Months")

## Standardize the moderator "Lag" to improve numerical stability and add it to the dataframe
Nohe.df$data$Lag <- scale(df$Lag)
    
head(Nohe.df$data)


## -----------------------------------------------------------------------------
## Create a matrix to present "Lag" moderator
## We use Lag to moderate the following paths:
## W1 -> W2
## W1 -> S2
## S1 -> W2
## S1 -> S2
A2 <- create.modMatrix(RAM=RAM3, output="A", mod="Lag")
A2

## Fit the model with Lag as a moderator
fit2 <- osmasem(model.name="Lag as a moderator", RAM=RAM1, Ax=A2, data=Nohe.df)
summary(fit2)

## Comparing the models with and without the covariate
anova(fit2, fit0)


## -----------------------------------------------------------------------------
## Effect of w2w when Lag is at the mean value
Mean <- mxEval(w2w, fit2$mx.fit)
Mean

## Effect of w2w when Lag is at +1 SD as Lag is standardized
PlusSD <- mxEval(w2w + w2w_1, fit2$mx.fit)
PlusSD

## Effect of w2w when Lag is at -1 SD as Lag is standardized
MinSD <- mxEval(w2w - w2w_1, fit2$mx.fit)
MinSD

## Show the effect graphically
plot(x=NULL, y=NULL, xlim=c(-1,1), ylim=c(-1,1), xlab="W1", ylab="W2", main="Slope of W2W conditional on Lag")
abline(a=0, b=MinSD, col="blue")
abline(a=0, b=Mean, col="red")
abline(a=0, b=PlusSD, col="green")
legend(-1, 1, c("-1 SD", "Mean", "+1 SD"), col=c("blue", "red", "green"), lty=c(1,1,1))


## ---- warning=TRUE------------------------------------------------------------
## Make a copy for illustrations
Nohe.miss <- Nohe.df

## Create missing values in the first two studies to illustrate the issues
Nohe.miss$data$Lag[c(1, 2)] <- NA

head(Nohe.miss$data)

## Fit the model with Lag as a moderator
## It does not work because of the missing data
fit3a <- osmasem(model.name="Lag as a moderator with NA", RAM=RAM1, Ax=A2, data=Nohe.miss)

## Create a variable to indicate which should be included (TRUE)
subset.rows <- c(!is.na(Nohe.miss$data$Lag))
subset.rows


## -----------------------------------------------------------------------------
## Fit the model with Lag as a moderator
fit3b <- osmasem(model.name="Lag as a moderator with NA", RAM=RAM1, Ax=A2, 
                 data=Nohe.miss, subset.rows=subset.rows)
summary(fit3b)

## Comparing the models with and without the covariate
## Incorrect result!!! The dfs is 16, which should be 4!
## It is because fit3b has fewer data points
anova(fit3b, fit0)


## -----------------------------------------------------------------------------
## Correct way to compare these two models
fit0.miss <- osmasem(model.name="No moderator but dropped data with NA in moderator", RAM=RAM1, 
                     data=Nohe.df, subset.rows=subset.rows)
anova(fit3b, fit0.miss)


## -----------------------------------------------------------------------------
## Show the original data
head(df)

## Compute correlations corrected for unreliability
## r_corr = r/√(rxx'*ryy')
df$`W1-S2cor` <- with(df, `W1-S2`/sqrt(RelW1)/sqrt(RelS2))
df$`S1-W2cor` <- with(df, `S1-W2`/sqrt(RelS1)/sqrt(RelW2))
df$`W1-S1cor` <- with(df, `W1-S1`/sqrt(RelW1)/sqrt(RelS1))
df$`W2-S2cor` <- with(df, `W2-S2`/sqrt(RelW2)/sqrt(RelS2))
df$`W1-W2cor` <- with(df, `W1-W2`/sqrt(RelW1)/sqrt(RelW2))
df$`S1-S2cor` <- with(df, `S1-S2`/sqrt(RelS1)/sqrt(RelS2))

## Variable names
my.var <- c("W1", "S1", "W2", "S2")

## Split each row as a list
my.list <- split(df, 1:nrow(df))


## -----------------------------------------------------------------------------
## Select the correlation coefficients and convert it into a correlation matrix
## my corrected correlations
my.corcor <- lapply(my.list, 
                 function(x) {## Use the names of the corrected correlations
                              mat <- vec2symMat(unlist(x[c("W1-S1cor","W1-W2cor","W1-S2cor","S1-W2cor","S1-S2cor","W2-S2cor")]), 
                                                diag = FALSE)
                              ## Add the dimensions for ease of reference
                              dimnames(mat) <- list(my.var, my.var)
                              ## Return the correlation matrix
                              mat})

## Add the study names for ease of reference
names(my.corcor) <- df$Study


## -----------------------------------------------------------------------------
rand1.cor <- tssem1(my.corcor, df$N, method="REM")
summary(rand1.cor)


## -----------------------------------------------------------------------------
rand2.cor <- tssem2(rand1.cor, RAM=RAM1)
summary(rand2.cor)

## Setup two plots side-by-side
layout(t(1:2))

## Plot the first group
plot(rand2a, col="green")
title("Uncorrected correlations")

## Plot the second group
plot(rand2.cor, col="yellow")
title("Corrected correlations")

