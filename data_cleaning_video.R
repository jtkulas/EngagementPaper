# Read data
inprogress <- read.csv("prolific data/inprogress.csv", header=TRUE, na.strings="", skip = 1) %>% 
  janitor::clean_names()

# Change column names
x <- paste("item", sep="",1:404)
colnames(inprogress) <- x

names(inprogress)

# Select columns
inprogress <- inprogress[,1:404]


# Sum NAs
inprogress <- inprogress %>% mutate(sum_NA = rowSums(is.na(inprogress[11:400])))

inprogress$sum_NA

# Freq distribution
hist(inprogress$sum_NA)

library(descr)
freq(inprogress$sum_NA)


# Keep people with less than 200 NAs
no_NA <- inprogress[which(inprogress$sum_NA < 200), ]

freq(no_NA$sum_NA)


# Look at histograms of each item for attention checks
freq(no_NA$item61)
freq(no_NA$item145)
freq(no_NA$item248)
freq(no_NA$item308)

# Attention checks
attention <- inprogress[which(inprogress$item61 == 5 & inproogress$item145 == 5 & inprogress$item248 == 2 & inprogress$item308 == 3), ]