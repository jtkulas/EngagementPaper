# Morgan factorizes the "What is the title of the job you were thinking about while responding to this survey?" column
# As well as the "How many hours do you typically work per week in this job?" column

# An object lesson in why you should NEVER HAVE FREE TEXT ENTRY IN YOUR SURVEY


# Packages
library(tidyverse)
library(DT)
library(labourR)


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

# Hours worked
together$`How many hours do you typically work per week in this job?` <- together$`How many hours do you typically work per week in this job?` %>% 
  gsub("[^0-9./-]", "", .) %>%  # removes all characters not included in brackets
  gsub(".*/", "", .) %>% # Selects second number after "/" (e.g. 40/50 -> 50)
  gsub(".*-", "", .) %>%  # Selects second number after "-" (e.g. 50-60 -> 60)
  gsub("[^0-9]", "", .) %>%
  str_sub(1,2) %>% # Subsets all responses to first two indices. Quick and easy way to rule out impossible hour entries
  as.numeric()

#Job title

#creates new column 'position' detecting presence of keywords. To create new search terms, copy the "str_detect" line
together <- together %>% 
  mutate_at(vars(`What is the title of the job you were thinking about while responding to this survey?`), 
            list(position =~ case_when(str_detect(., regex("account"))~"accountant",
                                       str_detect(., regex("analy"))~"analyst",
                                       TRUE~.)))

together$position <- together$`What is the title of the job you were thinking about while responding to this survey?` %>% 
  tolower() %>% 
  trimws()

#ESCO/ISCO classification using the labourR package
#ISCO = International Standard Classification of Occupations

together$id <- 1:nrow(together) #adds unique identifier to satisfy inane requirement of classify_occupation()

ISCO <- classify_occupation(together, text_col = 'position', isco_level = 1) #nrow = 249 (I think BC it ignores NA's?)

job_groups <- table(ISCO$iscoGroup) #table of first level of ISCO classifications for all participants

data.table(job_groups)
#1 (Managers): 51
#2 (Professionals): 120
#3 (Technicians and associate professionals): 62
#4 (Clerical support workers): 4
#5 (Service and sales workers): 8
#6 (Skilled agricultural, forest and fishery workers): 0
#7 (Craft and relate trades workers): 1
#8 (Plant and machine operators and assemblers): 3
#9 (Elementary occupations): 0
#0 (Armed forces occupations): 0

# I can't figure out how to merge this with the original dataframe because classify_occupation doesn't output an ID column that matches
# the original row number in together... for some reason. Either way this table gets the job done. Lesson learned: free text entry is bad

###### Tenure
#Result of manual coding
together$tenure <- c(0.5,7,2.5,0.5,0.83,14,3,3,5,2,2,12,3,1.5,5,1,5,1,3,20,1.5,NA,8,2,NA,4,2,4,1.08,5,15,3.5,
                     20,15,0.66,2,12.5,15,7,1.5,5,7,3.5,0.25,NA,15,NA,2,3,38,9,NA,2,1,NA,NA,1,0.20,NA,NA,2,
                     0.5,NA,NA,5,2,1,2,1.5,3,3,2,1.5,0.25,NA,1,NA,3,NA,3,1,5,0.83,1.5,0.08,16,4,2,9,11,8,
                     0.75,NA,4,3,4.5,NA,1.5,4,3,0.33,3,0.5,3,6.5,17,1,21,14,3,7,1,17,2.5,33,32,11,9,NA,2,3,1.5,1,NA,2.5,NA,NA,NA,NA,
                     NA,NA,NA,3,18,NA,2.5,NA,9,NA,8,15,12,2.5,20,2.5,17,10,20,6.5,6,1,10,2,0.83,6,0.33,8,1.16,
                     4,5,6,10,1.5,2,17,14,30,0.16,4,3,1,20,0.16,40,3,1,12,3.5,2.25,10,22,1,31,41,16,NA,35,
                     0.5,4,2.5,30,2,NA,4,2,NA,3,8,2,0.5,5,2.5,3,1.5,NA,0.33,4,NA,2,3,4,27,1.5,4,6,4,22,1.7,2,1.5,3.5,0.41,
                     7,6,1,0.08,1.5,7,0.41,4,2.5,2,2,12,7,0.66,NA,3,10,0.33,15,33,30,23,2,0.33,35,6,7,1,3,2,
                     1,3,10,NA,30,0.16,7,6,5,2,3,NA,NA,NA,4,2.5,4,NA,1.41,3,NA,NA,25,2,12,12,1.2,8,1.5,1
)

#creates demographic table using DT library

demo_table <- together %>% 
  select(`How many hours do you typically work per week in this job?`, tenure, 
         `What is the title of the job you were thinking about while responding to this survey?`) %>% 
  filter_all(any_vars(!is.na(.))) %>% #changed DT to filter out rows where all col values == NA
  datatable()

