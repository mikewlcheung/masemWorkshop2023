## ---- eval=FALSE, echo=FALSE, purl=TRUE---------------------------------------
## ## Install packages automatically if they are not available on your computer
## for (i in c("metaSEM", "symSEM", "lavaan", "semPlot", "readxl")) {
##   if (!(i %in% rownames(installed.packages()))) install.packages(i)
## }


## ---- echo=FALSE, fig.width=20, fig.height=6, out.width="90%"-----------------
par(mfrow = c(1,3))

pie(c(10), labels=c("Known sampling error"), col=rainbow(1), main="Fixed effect model: Total variance")  
pie(c(10, 8), labels=c("Known sampling error", "Between-study variance"), col=rainbow(2), 
    main="Random effect model: Total variance")  
pie(c(10, 3, 5), labels=c("Known sampling error", "Residual between-study variance", "Explained variance"), 
    col=rainbow(3), main="Mixed effect model: Total variance")  


## ---- message=FALSE-----------------------------------------------------------
## Library to conduct the meta-analysis
library(metaSEM)

## Show the first few cases
head(Jaramillo05)


## -----------------------------------------------------------------------------
## RE.constraints=0: tau^2=0
summary( meta(y=r, v=r_v, data=Jaramillo05, RE.constraints=0) )


## -----------------------------------------------------------------------------
## Random-effects model
model0 <- meta(y=r, v=r_v, data=Jaramillo05)
summary(model0)


## ---- message=FALSE-----------------------------------------------------------
## Center IDV for ease of interpretation: scale(IDV, scale=FALSE)
model1 <- meta(y=r, v=r_v, x=scale(IDV, scale=FALSE), data=Jaramillo05)
summary(model1)

