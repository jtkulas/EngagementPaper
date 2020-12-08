# Importing Data 

temp <- read.csv("qualtrics_pilot_data.csv", header=FALSE)
library(tidyverse)
glimpse(temp)

data <- as_tibble(temp)
glimpse(data)
library(dplyr)

# Remove rows 1 and 3
test <- data[-c(1,3),]

#Remove columns
test$V10 <- NULL
test$V11 <- NULL
test$V12 <- NULL
test$V13 <- NULL
 
# Adding a column

library(tibble)
add_column(test, .after = "V17") # we can change this to the name of the column

# ID column

test$id <- seq.int(nrow(test))
col_num <- grep("id", names(test))
test <- test[, c(col_num, (1:ncol(test))[-col_num])]

# Subsetting to 4 conditions

Condition1 <- test[, c(15:50)]
Condition2 <- test[, c(51:85)]
Condition3 <- test[, c(86:121)]
Condition4 <- test[, c(122:157)]
Demographic <- test[, c(159:163)]


# Blank values to filter out responses

Condition1.2 <- Condition1[!apply(Condition1 == "", 1, all), ]
Condition2.2 <- Condition2[!apply(Condition2 == "", 1, all), ]
Condition3.2 <- Condition3[!apply(Condition3 == "", 1, all), ]
Condition4.2 <- Condition4[!apply(Condition4 == "", 1, all), ]


# Filling in New column 


# Renaming Columns 

data2<-test %>%
   rename(Start.Date = V1, End.Date = V2, Status = V3, IPAddress = V4, Progress = V5, SecDuration = V6, 
        Finished = V7, RecordedDate = V8, ReponseID = V9, Latitude = V14, Longitude = V15, DistChannel = V16, 
        Language = V17, Item1 = V18, Item2 = V19)


