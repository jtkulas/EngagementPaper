## File(s) to try 18 items from 20

library(lavaan)
library(sem)
library(semPlot)
library(dplyr)

CFAdata <- read.csv("CFAdata.csv") 

testpoly <- CFAdata[2:37]


modified1 <-'
Absorption = ~Item_1 +  Item_2  + Item_3  + Item_5  + Item_8  + Item_10 + Item_11
Vigor      = ~Item_14 + Item_16 + Item_17 + Item_19 + Item_21 + Item_22
Dedication = ~Item_25 + Item_26 + Item_28 + Item_31 + Item_32 + Item_34 + Item_35 
Cognitive  = ~Item_1 +  Item_2  + Item_3  + Item_14 + Item_16 + Item_25 + Item_26 + Item_28
Affective  = ~Item_5 +  Item_8  + Item_17 + Item_19 + Item_31 + Item_32
Behavioral = ~Item_10 + Item_11 + Item_21 + Item_22 + Item_34 + Item_35
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor      ~~ 0*Affective
Vigor      ~~ 0*Behavioral
Vigor      ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
'

## https://stackoverflow.com/questions/41395611/cfa-in-r-lavaan-with-ordinal-data-polychoric-correlation-included

fit.poly <- lavCor(testpoly, ordered=TRUE, group=NULL, output="cov")


## Jorgensen's post for necessary poly elements to run a DWLS:

fit_poly <- lavCor(data = Data, ordered = TRUE, output = "lavaan")
## obtain summary stats
sample.cov  <- lavInspect(fit_poly, "sampstat")$cov
sample.mean <- lavInspect(fit_poly, "sampstat")$mean
sample.th   <- lavInspect(fit_poly, "sampstat")$th
attr(sample.th, "th.idx") <- lavInspect(fit_poly, "th.idx")
sample.nobs <- lavInspect(fit_poly, "nobs")
WLS.V <- lavInspect(fit_poly, "WLS.V")
NACOV <- lavInspect(fit_poly, "gamma")
## fit model to summary stats
fit <- cfa(model, sample.cov = sample.cov, sample.mean = sample.mean,
           sample.th = sample.th, sample.nobs = sample.nobs,
           WLS.V = WLS.V, NACOV = NACOV)

## end post


                   
Fit.mod1 <- lavaan::cfa(modified1, sample.cov=fit.poly, sample.nobs=282, estimator="DWLS") 

semPlot::semPaths(Fit.mod1, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0, edge.label.cex = 1.5, pastel=TRUE) 

modindices(Fit.mod1, sort = TRUE, maximum.number = 25)

summary(Fit.mod1, standardized=TRUE)    ## 340.76



#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
################################# Bifactor Here #############################################
#############################################################################################
#############################################################################################
#############################################################################################

## Still need to consider FIML here 

Sub_Model<-'
Absorption = ~Item_1 +  Item_2  + Item_3  + Item_5  + Item_8  + Item_10 + Item_11
Vigor      = ~Item_14 + Item_16 + Item_17 + Item_19 + Item_21 + Item_22
Dedication = ~Item_25 + Item_26 + Item_28 + Item_31 + Item_32 + Item_34 + Item_35 
'

Fit2.1<-lavaan::cfa(Sub_Model, data = CFAdata, missing = 'ML', estimator = 'MLR')


Att_Model<-'
Cognitive  = ~Item_1 +  Item_2  + Item_3  + Item_14 + Item_16 + Item_25 + Item_26 + Item_28
Affective  = ~Item_5 +  Item_8  + Item_17 + Item_19 + Item_31 + Item_32
Behavioral = ~Item_10 + Item_11 + Item_21 + Item_22 + Item_34 + Item_35
'
Fit2.2<-lavaan::cfa(Att_Model, data = CFAdata, missing = 'ML', estimator = 'MLR')

semPlot::semPaths(Fit2.1, "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)
semPlot::semPaths(Fit2.2, "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)

Fit2.1;Fit2.2



