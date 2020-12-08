## This is just a script to test out commands (so we don't have to build the book or knit individual chapters for data analyses)

temp <- read.csv("qualtrics_pilot_data.csv", header=FALSE, na.strings="")

x <- temp[2,]
data <- temp[-c(1:3),]
colnames(data) <- x

num <- nrow(data)

write.csv(table(data[165]), "comments.csv")
write.csv(table(data[164]), "tenure.csv")
table(data[163])

data$hours <- NULL
  
data$hours[data$`How many hours do you typically work per week in this job?` == "10-16"] <- 13
data$hours[data$`How many hours do you typically work per week in this job?` == "15"]    <- 15
data$hours[data$`How many hours do you typically work per week in this job?` == "16"]    <- 16
data$hours[data$`How many hours do you typically work per week in this job?` == "20"]    <- 20
data$hours[data$`How many hours do you typically work per week in this job?` == "25"]    <- 25
data$hours[data$`How many hours do you typically work per week in this job?` == "25-30"] <- 27.5
data$hours[data$`How many hours do you typically work per week in this job?` == "30"]    <- 30
data$hours[data$`How many hours do you typically work per week in this job?` == "30-40"] <- 35
data$hours[data$`How many hours do you typically work per week in this job?` == "32"]    <- 32

data$hours[data$`How many hours do you typically work per week in this job?` == "35"]    <- 35
data$hours[data$`How many hours do you typically work per week in this job?` == "35-40 hrs"]  <- 37.5
data$hours[data$`How many hours do you typically work per week in this job?` == "40"]    <- 40
data$hours[data$`How many hours do you typically work per week in this job?` == "40-45"] <- 42.5
data$hours[data$`How many hours do you typically work per week in this job?` == "40-50"] <- 45
data$hours[data$`How many hours do you typically work per week in this job?` == "40 "]   <- 40
data$hours[data$`How many hours do you typically work per week in this job?` == "40/50"] <- 45
data$hours[data$`How many hours do you typically work per week in this job?` == "43"]    <- 43

data$hours[data$`How many hours do you typically work per week in this job?` == "45"]    <- 45
data$hours[data$`How many hours do you typically work per week in this job?` == "45-50"] <- 47.5
data$hours[data$`How many hours do you typically work per week in this job?` == "45+ "]    <- 47.5
data$hours[data$`How many hours do you typically work per week in this job?` == "48"]    <- 48
data$hours[data$`How many hours do you typically work per week in this job?` == "50"]    <- 50
data$hours[data$`How many hours do you typically work per week in this job?` == "50-60"] <- 55
data$hours[data$`How many hours do you typically work per week in this job?` == "55"]    <- 55
data$hours[data$`How many hours do you typically work per week in this job?` == "6"]     <- NA

data$hours[data$`How many hours do you typically work per week in this job?` == "60"]    <- 60
data$hours[data$`How many hours do you typically work per week in this job?` == "60hr"]  <- 60
data$hours[data$`How many hours do you typically work per week in this job?` == "65"]    <- 65
data$hours[data$`How many hours do you typically work per week in this job?` == "9"]     <- NA
data$hours[data$`How many hours do you typically work per week in this job?` == " "]     <- NA

data$hours <- as.numeric(as.character(data$hours))
table(data$hours, data$`How many hours do you typically work per week in this job?`)

hist(data$hours)

library(ggplot2)

ggplot(data, aes(x = hours)) +           
  geom_histogram(alpha = 0.5, position = "identity") +
  geom_vline(aes(xintercept=mean(hours, na.rm=T)),   # Ignore NA values for mean
             color="blue", linetype="dotted", size=1) +
  labs(x="Average number of hours worked (per week)")

library(psych)
# temp <- describe(data)
# write.csv(temp, "checking.csv")

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

# write.csv(names(cond1), "cond1.csv")
# write.csv(names(cond2), "cond2.csv")
# write.csv(names(cond3), "cond3.csv")
# write.csv(names(cond4), "cond4.csv")

cond1.red <- cond1[,c(6, 18:53,162:165)]
cond2.red <- cond2[,c(6, )]


