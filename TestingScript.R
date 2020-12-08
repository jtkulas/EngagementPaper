## This is just a script to test out commands (so we don't have to build the book or knit individual chapters for data analyses)

temp <- read.csv("qualtrics_pilot_data.csv", header=FALSE)

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
describe(data)

data$Cond1 <- sum(is.na(data$`C1.1 - I am happiest when I am immersed in a project.`))

