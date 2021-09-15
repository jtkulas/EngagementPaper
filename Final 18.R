library(lavaan)
library(sem)
library(semPlot)
library(dplyr)

coviarance <- cov(CFAdata[2:37], use = 'na.or.complete')

Att_Model<-'
Cognitive=~Item_2+Item_3+Item_14+Item_15+Item_16+Item_26+ Item_28
Affective=~Item_5+Item_7+Item_8+Item_17+Item_19+Item_30+Item_31+Item_32
Behavioral=~Item_10+Item_11+Item_21+Item_22+Item_24+Item_34+Item_35+Item_36
'
Fit2.2<-lavaan::cfa(Att_Model, sample.cov = covariance, sample.nobs = 282)

semPlot::semPaths(Fit2.2, "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)

Fit2.2

#iteration 2 (cutting item 7 and 30)
Att_Model<-'
Cognitive=~Item_2+Item_3+Item_14+Item_15+Item_16+Item_26+ Item_28
Affective=~Item_5+Item_8+Item_17+Item_19+Item_31+Item_32
Behavioral=~Item_10+Item_11+Item_21+Item_22+Item_24+Item_34+Item_35+Item_36
'
Fit2.2<-lavaan::cfa(Att_Model, data = CFAdata, sample.nobs = 282, missing = "ML")

semPlot::semPaths(Fit2.2, "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNode = 0)

Fit2.2

#iteration 3 cutting item 24 + 36 (vigor behavioral)
Att_Model<-'
Cognitive=~Item_1+Item_3+Item_15+Item_16+Item_26+Item_28
Affective=~Item_5+Item_8+Item_17+Item_19+Item_31+Item_32
Behavioral=~Item_10+Item_11+Item_21+Item_22+Item_34+Item_35
'
Fit2.2<-lavaan::cfa(Att_Model, data = CFAdata, sample.nobs = 282)

semPlot::semPaths(Fit2.2, "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)

Fit2.2

#tentatively good on these 18 items within attitudinal
#testing on substantive model now

Sub_Model<-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_10+Item_11
Vigor=~Item_15+Item_16+Item_17+Item_19+Item_21+Item_22
Dedication=~Item_26+Item_28+Item_31+Item_32+Item_34+Item_35
'

Fit2.1<-lavaan::cfa(Sub_Model, data = CFAdata, sample.nobs = 282)

semPlot::semPaths(Fit2.1, "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)

Fit2.1

modindices(Fit2.1, sort = TRUE) #cross-loadings: Absorption: 16 (vigor; mi = 26.8), 26 (Dedication; mi = 17), 19 (vigor; mi = 13.9)
modindices(Fit2.2, sort = TRUE) #cross-loadings: behavioral: 19 (affective; mi = 51.2): cognitive: item 35 (behavioral; mi = 17.8)



#Bifactor
modified1 <-'
Absorption=~Item_1+Item_3+Item_5+Item_8+Item_10+Item_11
Vigor=~Item_15+Item_16+Item_17+Item_19+Item_21+Item_22
Dedication=~Item_26+Item_28+Item_31+Item_32+Item_34+Item_35
Cognitive=~Item_1+Item_3+Item_15+Item_16+Item_26+Item_28
Affective=~Item_5+Item_8+Item_17+Item_19+Item_31+Item_32
Behavioral=~Item_10+Item_11+Item_21+Item_22+Item_34+Item_35
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

Fit.mod1 <- lavaan::cfa(modified1, data = CFAdata) 

semPlot::semPaths(Fit.mod1, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0, edge.label.cex = 1.5, pastel=TRUE) 

modindices(Fit.mod1, sort = TRUE, maximum.number = 25)

summary(Fit.mod1, standardized=TRUE)

Fit.mod1
