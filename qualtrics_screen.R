## Cleaning Qualtrics construct validation data - 10/14/21

newdata.att <- read.csv("Engagement+(Attitudinal)_October+12,+2021_08.02.csv")#[47:66]
newdata.sub <- read.csv("Engagement+(Substantive)_October+12,+2021_08.01.csv")#[47:66]

newdata.att <- newdata.att[-c(1:2),]                      ## peskies
newdata.sub <- newdata.sub[-c(1:2),]

write.csv(newdata.att, "changingnumeric.csv")
newdata.att <- read.csv("changingnumeric.csv")            ## that worked!! 
write.csv(newdata.sub, "changingnumeric.csv")
newdata.sub <- read.csv("changingnumeric.csv") 

library(careless)
newdata.att$careless_long <- longstring(newdata.att[20:83])
newdata.sub$careless_long <- longstring(newdata.sub[20:83])
newdata.att$irv <- irv(newdata.att[48:51])
newdata.sub$irv <- irv(newdata.sub[55:58])

newdata.att$irv2 <- irv(newdata.att[25:28])   ## another reverse-score
newdata.sub$irv2 <- irv(newdata.sub[25:28])

## newdata.att$missing <- rowSums(is.na(newdata.att[20:83])) 
## newdata.sub$missing <- rowSums(is.na(newdata.sub[20:83])) 
descr::freq(newdata.att$irv2)
descr::freq(newdata.sub$irv2)

## need filter for non-differentiating reverse-score people

newdata.att$flag <- "use"; newdata.sub$flag <- "use"         ## need to do because subset on lines 44/45 can't != "Flagged"

newdata.att$flag[newdata.att$careless_long > 12 | newdata.att$Duration..in.seconds. < 200 | newdata.att$irv == 0 | newdata.att$irv2 == 0]  <- "Flagged"
newdata.sub$flag[newdata.sub$careless_long > 12 | newdata.sub$Duration..in.seconds. < 200 | newdata.sub$irv == 0 | newdata.sub$irv2 == 0]  <- "Flagged"

write.csv(newdata.att, "attitudeset.csv")
write.csv(newdata.sub, "substantiveset.csv")

newdata.att$flag <- as.factor(newdata.att$flag)
newdata.sub$flag <- as.factor(newdata.sub$flag)

descr::freq(newdata.att$flag)
descr::freq(newdata.sub$flag)

useatt <- newdata.att[ which(newdata.att$flag == "use"), ]
usesub <- newdata.sub[ which(newdata.sub$flag == "use"), ]

psych::alpha(useatt[c(48:51)])
