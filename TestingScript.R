## This is just a script to test out commands (so we don't have to build the book or knit individual chapters for data analyses)

temp <- read.csv("FINAL+Engagement+Pilot_November+16,+2020_08.45.csv", header=FALSE)

x <- temp[2,]
data <- temp[-c(1:3),]
colnames(data) <- x

num <- nrow(data)

table(data[165])
table(data[164])
table(data[163])
