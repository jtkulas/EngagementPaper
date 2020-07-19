cvalid <- read.csv("C:\\Kulas\\Content Validity\\Engagement Survey Content Validity.csv")

## Absorption
table(cvalid[,c(6,2)])
table(cvalid[,c(6,3)])
table(cvalid[,c(6,4)])

## Vigor
table(cvalid[,c(7,2)])
table(cvalid[,c(7,3)])
table(cvalid[,c(7,4)])

## Dedication
table(cvalid[,c(8,2)])
table(cvalid[,c(8,3)])
table(cvalid[,c(8,4)])
