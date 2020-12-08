# Importing Data 

temp <- read.csv("qualtrics_pilot_data.csv", header=TRUE, na.strings="")

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


# NA values to filter out responses
Con1.2 <- na.omit(Con1)
Con2.2 <- na.omit(Con2)
Con3.2 <- na.omit(Con3)
Con4.2 <- na.omit(Con4)

# This doesn't seem to be working 
# Con1.2 <- Con1[!apply(Condition1 == "NA", 1, all), ]
# Con2.2 <- Con2[!apply(Condition2 == "NA", 1, all), ]
# Con3.2 <- Con3[!apply(Condition3 == "NA", 1, all), ]
# Con4.2 <- Con4[!apply(Condition4 == "NA", 1, all), ]

# Condition column
Con1.2$Condition <- c("Condition1")
Con2.2$Condition <- c("Condition2")
Con3.2$Condition <- c("Condition3")
Con4.2$Condition <- c("Condition4")

# Merge Data files 
library(dplyr)
merge <- full_join(Con1.2, Con2.2, by = "Condition")
merge2 <- full_join(Con3.2, Con4.2, by = "Condition")
FullMerge <- full_join(merge, merge2, by = "Condition")

# Move column to the front
col_idx <- grep("Condition", names(FullMerge))
FullMerge <- FullMerge[, c(col_idx, (1:ncol(FullMerge))[-col_idx])]










