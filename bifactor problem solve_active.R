# building the bifactor - 8/6/21
# write.csv(CFAdata, "CFAdata.csv")     ## recoded
## Morgan and Kulas wrote the CFAdata object to a .csv 8/6/21

library(lavaan)
library(sem)
library(semPlot)
library(dplyr)

CFAdata <- read.csv("CFAdata.csv") 

## delete: 4, 12
## 33, 9, 13
## 18, 20, 29, 25 & 27 (unique because mod indices wanted to free 25&27 OR 26&28)
## 6
## 23 due to factor loading not modification indices
## 1


Sub_Model<-'
Absorption=~Item_2+Item_3+Item_7+Item_8+Item_10+Item_11
Vigor=~Item_14+Item_15+Item_16+Item_17+Item_19+Item_21+Item_22+Item_24
Dedication=~Item_26+ Item_28+Item_30+Item_31+Item_32+Item_34+Item_35+Item_36
'

Fit2.1<-lavaan::cfa(Sub_Model, data = CFAdata)
write.csv(modindices(Fit2.1, sort = TRUE, maximum.number = 25), "sub2.csv")


Att_Model<-'
Cognitive=~Item_2+Item_3+Item_14+Item_15+Item_16+Item_26+ Item_28
Affective=~Item_5+Item_7+Item_8+Item_17+Item_19+Item_30+Item_31+Item_32
Behavioral=~Item_10+Item_11+Item_21+Item_22+Item_24+Item_34+Item_35+Item_36
'
Fit2.2<-lavaan::cfa(Att_Model, data = CFAdata)
write.csv(modindices(Fit2.2, sort = TRUE, maximum.number = 25), "att2.csv")

semPlot::semPaths(Fit2.1, "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)
semPlot::semPaths(Fit2.2, "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)

Fit2.1;Fit2.2

#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
################################# Bifactor Here #############################################
#############################################################################################
#############################################################################################
#############################################################################################

## delete items 4, 12 
## delete 33, 9, 13
## 18, 20, 29


## deleted if: 1) modification index was high (relative to others) and 2) error residual was within same cell 

## 14 seems important - deleted 8/10 but might want to reconsider at final decision

modified1 <-'
Absorption=~Item_3+Item_2+Item_5+Item_7+Item_8+Item_10
Vigor=~Item_15+Item_16+Item_17+Item_21+Item_22+Item_24
Dedication=~Item_26+ Item_28 +Item_30+Item_31+Item_32+Item_34+Item_35+Item_36
Cognitive=~Item_3+Item_2+Item_15+Item_16+ Item_26+ Item_28
Affective=~Item_5+Item_7+Item_8+Item_17+Item_30+Item_31+Item_32
Behavioral=~Item_10+Item_21+Item_22+Item_24+Item_34+Item_35+Item_36
Absorption ~~ 0*Affective
Absorption ~~ 0*Behavioral
Absorption ~~ 0*Cognitive
Vigor ~~ 0*Affective
Vigor ~~ 0*Behavioral
Vigor ~~ 0*Cognitive
Dedication ~~ 0*Affective
Dedication ~~ 0*Behavioral
Dedication ~~ 0*Cognitive
Item_16 ~~ 1*Item_16         ## was (small) Heywood case
'

# Dedication ~~ 0*Vigor      
# Dedication ~~ 0*Absorption
# Vigor ~~ 0*Absorption
# Affective ~~ 0*Behavioral
# Affective ~~ 0*Cognitive
# Behavioral ~~ 0*Cognitive
#'

Fit.mod1 <- lavaan::cfa(modified1, data = CFAdata) 

semPlot::semPaths(Fit.mod1, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0, edge.label.cex = 1.5, pastel=TRUE) 

modindices(Fit.mod1, sort = TRUE, maximum.number = 25)

summary(Fit.mod1, standardized=TRUE)    ## 340.76

## write.csv(psych::describe(CFAdata), "descriptives.csv")

#library(tidyverse)

#write.csv(CFAdata %>% 
#  select(Item_1,Item_3,Item_13,Item_16,Item_26,Item_28,Item_5,Item_8,Item_17,Item_18,Item_29,Item_32,Item_9,Item_10,Item_22,Item_23,Item_33,Item_34,Item_29,Item_30, Item_31, Item_32) %>% 
#  cor(use = "complete.obs"), "smallcor.csv")

## Casey's below

#Bifactor_Model<-'
#Cognitive=~Item_1+Item_3+Item_13+Item_16+Item_26+Item_28
#Affective=~Item_5+Item_8+Item_17+Item_18+Item_30+Item_31
#Behavioral=~Item_9+Item_10+Item_22+Item_23+Item_33+Item_34
#Absorption=~Item_1+Item_3+Item_5++Item_8+Item_9+Item_10
#Vigor=~Item_13+Item_16+Item_17+Item_18+Item_22+Item_23
#Dedication=~Item_26+Item_28+Item_30+Item_31+Item_33+Item_34
#Absorption ~~ 0*Affective
#Absorption ~~ 0*Behavioral
#Absorption ~~ 0*Cognitive
#Vigor ~~ 0*Affective
#Vigor ~~ 0*Behavioral
#Vigor ~~ 0*Cognitive
#Dedication ~~ 0*Affective
#Dedication ~~ 0*Behavioral
#Dedication ~~ 0*Cognitive
#'


#casey.mod1 <- lavaan::cfa(Bifactor_Model, data = CFAdata) 

#semPlot::semPaths(casey.mod1, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
#                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0, edge.label.cex = 1.5, pastel=TRUE) 

