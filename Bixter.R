
CFAdata <- read.csv("CFAdata.csv")

poly <- psych::polychoric(CFAdata)
poly$rho

## Bifactor below (5/19/21):

## Model specs:
Bifactor_Model<-'
Cognitive=~Item_1+Item_2+Item_3+Item_4+Item_13+Item_14+Item_15+Item_16+Item_25+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_7+Item_8+Item_17+Item_18+Item_19+Item_20+Item_29+Item_30+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_11+Item_12+Item_21+Item_22+Item_23+Item_24+Item_33+Item_34+Item_35+Item_36
Absorption=~Item_1+Item_2+Item_3+Item_4+Item_5+Item_6+Item_7+Item_8+Item_9+Item_10+Item_11+Item_12
Vigor=~Item_13+Item_14+Item_15+Item_16+Item_17+Item_18+Item_19+Item_20+Item_21+Item_22+Item_23+Item_24
Dedication=~Item_25+Item_26+Item_27+Item_28+Item_29+Item_30+Item_31+Item_32+Item_33+Item_34+Item_35+Item_36
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor ~~ 0*Affective
Vigor ~~ 0*Behavioral
Vigor ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
'

## Thread on correlations as input and need to constrain residual variances: https://groups.google.com/g/lavaan/c/sJuzT7obIpg?pli=1

Fit.Bi <- lavaan::cfa(Bifactor_Model, data = CFAdata) ## Absorption & Vigor have correlation over 1
Fit.Bi <- lavaan::cfa(Bifactor_Model, sample.cov = poly$rho, sample.nobs = 282, estimator = "GLS")
Fit.Bi <- lavaan::cfa(Bifactor_Model, data = CFAdata, estimator = "MLR")
modindices(Fit.Bi, sort = TRUE, maximum.number = 5)
summary(Fit.Bi)


## Model visual:
semPlot::semPaths(Fit.Bi, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (36 candidate items)")
