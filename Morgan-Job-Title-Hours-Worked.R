# Morgan factorizes the "What is the title of the job you were thinking about while responding to this survey?" column
# As well as the "How many hours do you typically work per week in this job?" column

# An object lesson in why you should NEVER HAVE FREE TEXT ENTRY IN YOUR SURVEY


# Packages
library(tidyverse)
library(DT)


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
unique(together$`How many hours do you typically work per week in this job?`) # Sixty-two unique responses

gsub("[^0-9.-]", "", together$`How many hours do you typically work per week in this job?`)

together$`How many hours do you typically work per week in this job?` 
  
together$`How many hours do you typically work per week in this job?` <- together$`How many hours do you typically work per week in this job?` %>% 
  gsub("[^0-9./-]", "", .) %>%  # removes all characters not included in brackets
  gsub(".*/", "", .) %>% # Selects second number after "/" (e.g. 40/50 -> 50)
  gsub(".*-", "", .) %>%  # Selects second number after "-" (e.g. 50-60 -> 60)
  gsub("[^0-9]", "", .) %>% # Removes
  str_sub(1,2) %>% # Subsets all responses to first two indices. Quick and easy way to rule out impossible hour entries
  as.numeric()

# Organizational tenure
# This is going to be a doozy and will require some imperfect text analysis

together$tenure <- together[40]
  



hold <- head(together[40]) %>%  #%>% #test data for playing with temp
  as.vector() 

months <- str_detect(hold[,1], "month") #logical vector for whether row contains month
years <- str_detect(hold[,1], "year") #logical vector for whether row contains year

#and no clue how to do more than this



#Job title
unique(together$`What is the title of the job you were thinking about while responding to this survey?`) # Two-hundred and thirty-seven unique responses


together$`What is the title of the job you were thinking about while responding to this survey?`


#ONET job clusters, for use as factor levels
onet <- c('Agriculture, Food & Natural Resources', 'Archtecture & Construction', 'Arts, Audio/Video Technology & Communications',
          'Business Management & Administration', 'Education & Training', 'Finance', 'Government & Public Administration',
          'Health Science', 'Hospitality & Tourism', 'Human Services', 'Information Technology', 'Law, Public Safety, Corrections & Security',
          'Manufacturing', 'Marketing', 'Science, Technology, Engineering & Mathematics', 'Transportation, Distribution & Logistics')



#creates new column 'position' detecting presence of keywords. To create new search terms, copy the "str_detect" line
together <- together %>% 
  mutate_at(vars(`What is the title of the job you were thinking about while responding to this survey?`), 
            list(position =~ case_when(str_detect(., regex("account"))~"accountant",
                                       str_detect(., regex("analy"))~"analyst",
                                       TRUE~.)))

together$position %>% 
  tolower() %>% 
  trimws() %>% 
  table()



#creates demographic table using DT library

demo_table <- together[38:40] %>% 
  filter_all(any_vars(!is.na(.))) %>% #changed DT to filter out rows where all col values == NA
  datatable()



# To do:
# Find quick and easy classification

# include as appendix in SIOPpapaja
# make as searchable table inside tech report via "DT" package
#     May only work inside HTML version
# include other demo data (e.g. hours worked)

# Down the line:
# management/non management






