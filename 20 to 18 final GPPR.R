## File(s) to try 18 items from 20

library(lavaan)
library(sem)
library(semPlot)
library(dplyr)

CFAdata <- read.csv("CFAdata.csv") 


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

Fit.mod1 <- lavaan::cfa(modified1, data = CFAdata, missing = 'ML', estimator = 'MLR') 

semPlot::semPaths(Fit.mod1, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0, edge.label.cex = 1.5, pastel=TRUE) 

modindices(Fit.mod1, sort = TRUE, maximum.number = 25)

summary(Fit.mod1, standardized=TRUE)    ## 340.76



#############################################################################################


modified2 <-'
Absorption = ~Item_1 + Item_3  + Item_5  + Item_8  + Item_10 + Item_11
Vigor      = ~Item_14 + Item_16 + Item_17 + Item_19 + Item_21 + Item_22
Dedication = ~ Item_26 + Item_28 + Item_31 + Item_32 + Item_34 + Item_35 
Cognitive  = ~Item_1  + Item_3  + Item_14 + Item_16 + Item_26 + Item_28
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

Fit.mod2 <- lavaan::cfa(modified2, data = CFAdata, missing = "ML", estimator = 'MLR') 

semPlot::semPaths(Fit.mod2, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0, pastel=TRUE) 

modindices(Fit.mod2, sort = TRUE, maximum.number = 25)

summary(Fit.mod2, standardized=TRUE)


## removed items #2 and #25 which resulted in a factor loading of -0.10 to -.006 for item 10


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



