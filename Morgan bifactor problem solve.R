#CFA Analysis

temp <- read.csv("qualtrics_pilot_data.csv", header=FALSE, na.strings="")

x <- temp[2,]
data <- temp[-c(1:3),]
colnames(data) <- x

num <- nrow(data)

## getting conditions into one large file below - 12/9/20

data$Cond1 <- rowSums(is.na(data[18:53]))
data$Cond2 <- rowSums(is.na(data[54:89]))
data$Cond3 <- rowSums(is.na(data[90:125]))
data$Cond4 <- rowSums(is.na(data[126:161]))

data$Condition[data$Cond1 < 36] <- 1
data$Condition[data$Cond2 < 36] <- 2
data$Condition[data$Cond3 < 36] <- 3
data$Condition[data$Cond4 < 36] <- 4

cond1 <- data[ which(data$Condition==1), ]
cond2 <- data[ which(data$Condition==2), ]
cond3 <- data[ which(data$Condition==3), ]
cond4 <- data[ which(data$Condition==4), ]

cond1.red <- cond1[,c(6, 18:53, 162:165, 171)]  ## using Cond1 ordering
cond2.red <- cond2[,c(6, 62:65, 70:73, 82:85, 58:61, 74:77, 86:89, 66:69, 78:81, 54:57, 162:165, 171)]
cond3.red <- cond3[,c(6, 94:97, 106:109, 118:121, 98:101, 110:113, 122:125, 102:105, 114:117, 90:93, 162:165, 171)]
cond4.red <- cond4[,c(6, 138:161, 130:137, 126:129, 162:165, 171)]        ## 171 versus 172 because testing script has extra "hours" variable

names(cond1.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond1.red))      ## Getting rid of condition markers so rbind will work
names(cond2.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond2.red))  
names(cond3.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond3.red))  
names(cond4.red) = gsub(pattern = "C*.* - ", replacement = "", x = names(cond4.red))  

together <- rbind(cond1.red, cond2.red, cond3.red, cond4.red)        ## we'll be using this object for analyses

num_valid <- nrow(together)

i <- c(1:37)                                          ## Changing item responses to numerics
together[ , i] <- apply(together[ , i], 2,            # Specify own function within apply
                        function(x) as.numeric(as.character(x)))


##########################################################
##########################################################
##########################################################
############# RECODES

together$`Most days, I feel happiest when the workday is soon to be complete.` <- 7 - together$`Most days, I feel happiest when the workday is soon to be complete.`
together$`This job drains my energy.` <- 7 - together$`This job drains my energy.`

## BEHAVIORAL (NONE):

## COGNITIVE: 

together$`Thinking about work saps my energy.` <- 7 - together$`Thinking about work saps my energy.`
together$`I often think about finding another job.` <- 7 - together$`I often think about finding another job.`


library(lavaan)
library(sem)
library(semPlot)
library(dplyr)


CFAdata<-together[,2:37]


CFAdata<-CFAdata%>%rename(
  Item_1=`Iâ€™m able to concentrate on my work without distractions.`,
  Item_2=`I have a hard time detaching mentally from my work.`,
  Item_3=`Time passes quickly while Iâ€™m working.`,
  Item_4=`I find it difficult to mentally disconnect from work.`,
  Item_5=`I enjoy thinking about work even when Iâ€™m not at work.`,
  Item_6=`Most days, I feel happiest when the workday is soon to be complete.`,
  Item_7=`I am happiest when I am immersed in a project.`,
  Item_8=`I love starting my workday.`,
  Item_9=`I devote more time than is expected of me.`,
  Item_10=`I have to be reminded to take breaks while Iâ€™m at work.`,
  Item_11=`I never miss a work deadline.`,
  Item_12=`I never allow distractions to interfere with my work.`,
  Item_13=`I devote my full attention to my work tasks throughout the day.`,
  Item_14=`Thinking about work saps my energy.`,
  Item_15=`I would rather direct my focus toward a work task than a personal task.`,
  Item_16=`Iâ€™m able to maintain good levels of energy throughout the workday.`,
  Item_17=`I enjoy spending time completing my job tasks.`,
  Item_18=`Most days I feel enthusiastic about starting my work day.`,
  Item_19=`I feel motivated to go beyond what is asked of me.`,
  Item_20=`This job drains my energy.`,
  Item_21=`When work is slow I find ways to be productive.`,
  Item_22=`I express enthusiasm for my job while at work.`,
  Item_23=`I try my best to perform well at work.`,
  Item_24=`If I notice my energy level is low, I take corrective steps to re-energize.`,
  Item_25=`I plan my future with this company.`,
  Item_26=`I believe this company cares about my career goals.`,
  Item_27=`I often think about finding another job.`,
  Item_28=`This organization challenges me to work at my full potential.`,
  Item_29=`I am proud to be a member of this organization.`,
  Item_30=`I feel supported by my supervisor when I fail at a task.`,
  Item_31=`I feel proud of my accomplishments within this organization.`,
  Item_32=`My job makes me feel like Iâ€™m part of something meaningful.`,
  Item_33=`I make valued contributions to the organization.`,
  Item_34=`I embrace challenging situations at work.`,
  Item_35=`I speak positively about this organization to others.`,
  Item_36=`This organization provides the resources necessary for me to successfully perform my job.`
)

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################


Sub_Model<-'
Absorption=~Item_1+Item_2+Item_3+Item_4+Item_5+Item_6+Item_7+Item_8+Item_9+Item_10+Item_11+Item_12
Vigor=~Item_13+Item_14+Item_15+Item_16+Item_17+Item_18+Item_19+Item_20+Item_21+Item_22+Item_23+Item_24
Dedication=~Item_25+Item_26+Item_27+Item_28+Item_29+Item_30+Item_31+Item_32+Item_33+Item_34+Item_35+Item_36
'

Fit1.1<-lavaan::cfa(Sub_Model, data = CFAdata)
write.csv(modindices(Fit1.1, sort = TRUE, maximum.number = 25), "sub.csv")


Att_Model<-'
Cognitive=~Item_1+Item_2+Item_3+Item_4+Item_13+Item_14+Item_15+Item_16+Item_25+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_7+Item_8+Item_17+Item_18+Item_19+Item_20+Item_29+Item_30+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_11+Item_12+Item_21+Item_22+Item_23+Item_24+Item_33+Item_34+Item_35+Item_36
'
Fit1.2<-lavaan::cfa(Att_Model, data = CFAdata)
write.csv(modindices(Fit1.2, sort = TRUE, maximum.number = 25), "att.csv")


#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
############################# New mod indices below #################################
#####################################################################################
#####################################################################################
#####################################################################################

## delete: 2, 12
## 9,33

## write.csv(cor(CFAdata, use="pairwise.complete.obs"), "correlations.csv")

Sub_Model<-'
Absorption=~Item_1+Item_3+Item_4+Item_5+Item_6+Item_7+Item_8+Item_10+Item_11
Vigor=~Item_13+Item_14+Item_15+Item_16+Item_17+Item_18+Item_19+Item_20+Item_21+Item_22+Item_23+Item_24
Dedication=~Item_25+Item_26+Item_27+Item_28+Item_29+Item_30+Item_31+Item_32+Item_34+Item_35+Item_36
'

Fit2.1<-lavaan::cfa(Sub_Model, data = CFAdata)
write.csv(modindices(Fit2.1, sort = TRUE, maximum.number = 25), "sub2.csv")


Att_Model<-'
Cognitive=~Item_1+Item_3+Item_4+Item_13+Item_14+Item_15+Item_16+Item_25+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_7+Item_8+Item_17+Item_18+Item_19+Item_20+Item_29+Item_30+Item_31+Item_32
Behavioral=~Item_10+Item_11+Item_21+Item_22+Item_23+Item_24+Item_34+Item_35+Item_36
'
Fit2.2<-lavaan::cfa(Att_Model, data = CFAdata)
write.csv(modindices(Fit2.2, sort = TRUE, maximum.number = 25), "att2.csv")

semPlot::semPaths(Fit2.2, "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################

## Bifactor below (5/19/21): ######## THIS IS FULL 36-item PROBLEMATIC SPECIFICATION ##########

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
#Cognitive ~~ Affective
#Cognitive ~~ Behavioral
#Affective ~~ Behavioral
#Absorption ~~ Vigor
#Absorption ~~ Dedication
#Vigor ~~ Dedication'

Fit.Bi <- lavaan::cfa(Bifactor_Model, data = CFAdata) #, likelihood = "wishart")

semPlot::semPaths(Fit.Bi, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0)  ## exoCov=FALSE deletes all covariances
title("Initial pilot bifactor analysis (36 candidate items)")

modindices(Fit.Bi, sort = TRUE, maximum.number = 5)

summary(Fit.Bi)



#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
################################# Minus 2 and 12 ############################################
#############################################################################################
#############################################################################################
#############################################################################################

## delete items 2, 12 (remember to do twice on all iterations)
#MORGAN: Don't edit this--use the copied version of this below
modified1 <-'
Cognitive=~Item_1+Item_3+Item_2+Item_13+Item_14+Item_15+Item_16+Item_25+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_7+Item_8+Item_17+Item_18+Item_19+Item_20+Item_29+Item_30+Item_31+Item_32
Behavioral=~Item_9+Item_10+Item_21+Item_22+Item_23+Item_24+Item_33+Item_34+Item_35+Item_36
Absorption=~Item_1+Item_3+Item_2+Item_5+Item_6+Item_7+Item_8+Item_9+Item_10
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
#Dedication ~~ 0*Vigor      
#Dedication ~~ 0*Absorption
#Vigor ~~ 0*Absorption
#Affective ~~ 0*Behavioral
#Affective ~~ 0*Cognitive
#Behavioral ~~ 0*Cognitive
#'

Fit.mod1 <- lavaan::cfa(modified1, data = CFAdata) 

semPlot::semPaths(Fit.mod1, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0) 

modindices(Fit.mod1, sort = TRUE, maximum.number = 25)

summary(Fit.mod1)


#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
################################# Play Here #################################################
#############################################################################################
#############################################################################################
#############################################################################################

## delete items 2, 12 
## delete 33, 9

## deleted if: 1) modification index was high (relative to others) and 2) error residual was within same cell 

modified1 <-'
Cognitive=~Item_1+Item_3+Item_2+Item_14+Item_15+Item_16+Item_25+Item_26+Item_27+Item_28
Affective=~Item_5+Item_6+Item_7+Item_8+Item_19+Item_20+Item_29+Item_30+Item_31+Item_32
Behavioral=~Item_10+Item_21+Item_22+Item_23+Item_24+Item_34+Item_35+Item_36
Absorption=~Item_1+Item_3+Item_2+Item_5+Item_6+Item_7+Item_8+Item_10
Vigor=~Item_14+Item_15+Item_16+Item_19+Item_20+Item_21+Item_22+Item_23+Item_24
Dedication=~Item_25+Item_26+Item_27+Item_28+Item_29+Item_30+Item_31+Item_32+Item_34+Item_35+Item_36
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

#Dedication ~~ 0*Vigor      
#Dedication ~~ 0*Absorption
#Vigor ~~ 0*Absorption
#Affective ~~ 0*Behavioral
#Affective ~~ 0*Cognitive
#Behavioral ~~ 0*Cognitive
#'

Fit.mod1 <- lavaan::cfa(modified1, data = CFAdata) 

semPlot::semPaths(Fit.mod1, bifactor = c("Cognitive", "Affective", "Behavioral"), "std", layout = "tree3", 
                  rotation = 2, curvePivot=TRUE, style="lisrel", nCharNodes = 0) 

modindices(Fit.mod1, sort = TRUE, maximum.number = 25)

summary(Fit.mod1)

## write.csv(psych::describe(CFAdata), "descriptives.csv")
