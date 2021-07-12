# Morgan factorizes the "What is the title of the job you were thinking about while responding to this survey?" column
# As well as the "How many hours do you typically work per week in this job?" column

# An object lesson in why you should NEVER HAVE FREE TEXT ENTRY IN YOUR SURVEY


# Packages
library(tidyverse)
library(DT)


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


#Job title
unique(together$`What is the title of the job you were thinking about while responding to this survey?`) # Two-hundred and thirty-seven unique responses


together$`What is the title of the job you were thinking about while responding to this survey?`


#ONET job clusters, for use as factor levels
onet <- c('Agriculture, Food & Natural Resources', 'Archtecture & Construction', 'Arts, Audio/Video Technology & Communications',
          'Business Management & Administration', 'Education & Training', 'Finance', 'Government & Public Administration',
          'Health Science', 'Hospitality & Tourism', 'Human Services', 'Information Technology', 'Law, Public Safety, Corrections & Security',
          'Manufacturing', 'Marketing', 'Science, Technology, Engineering & Mathematics', 'Transportation, Distribution & Logistics')



together$`What is the title of the job you were thinking about while responding to this survey?` %>% 
  tolower() %>% 
  trimws() 
  


# To do:
# Find quick and easy classification

# include as appendix in SIOPpapaja
# make as searchable table inside tech report via "DT" package
#     May only work inside HTML version
# include other demo data (e.g. hours worked)

# Down the line:
# management/non management






