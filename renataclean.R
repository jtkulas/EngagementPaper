# Importing Data 

temp <- read.csv("qualtrics_pilot_data.csv", header=TRUE, na.strings="")
library(tidyverse)
glimpse(temp)

# Libraries 

library(stringr)
library(tidyverse)
library(lavaan)
library(sem)
library(semPlot)
library(ggplot2)

data <- temp
glimpse(data)
library(dplyr)

# Remove columns and rows 1 and 3
data2 <- data[-c(1:3),-c(1:17)]
test <- data2[-c(1,3),]

# Split the Data by Condition

library(psych)

Con1<-test %>% select(C1.1_1:C1.9_4)
Con1<-na.omit(Con1)
Con2<-test %>% select(C2.1_1:C2.9_4)
Con2<-na.omit(Con2)
Con3<-test %>% select(C3.1_1:C3.3_12)
Con3<-na.omit(Con3)
Con4<-test %>% select(C4.1_1:C4.3_12)
Con4<-na.omit(Con4)


# Changing to numeric

Con1<-as.data.frame(apply(Con1,2, as.numeric))
Con2<-as.data.frame(apply(Con2,2, as.numeric))
Con3<-as.data.frame(apply(Con3,2, as.numeric))
Con4<-as.data.frame(apply(Con4,2, as.numeric))


# Condition column




# Blank values to filter out responses

Condition1.2 <- Condition1[!apply(Condition1 == "", 1, all), ]
Condition2.2 <- Condition2[!apply(Condition2 == "", 1, all), ]
Condition3.2 <- Condition3[!apply(Condition3 == "", 1, all), ]
Condition4.2 <- Condition4[!apply(Condition4 == "", 1, all), ]


# Filling in New column 



