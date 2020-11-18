# Engage Pilot Choice Text
df_CH <- read.csv("Engage_Pilot_Choice.csv", header=FALSE,na.strings="")


  # Removing Uncessary Data
    df2_CH<-df_CH[-c(3),]
    df2_CH<-df2_CH[-c(1:17)]

  # Saving Headers
    header<-df2_CH[1:2,]
    
  # Recoding Values 

    df2_CH[ paste0("V", 18:161) ] <- 
      lapply( df2_CH[ , grepl("^V", names(df2_CH))] ,  
          function(x) {c('Strongly Disagree'=1, 'Disagree'=2, 'Slightly Disagree'=3, 'Slightly Agree'=4, 'Agree'=5, 'Strongly Agree'=6 )[as.character(x)]})

   # Combining Headers and Values
    
    df2_CH<-df2_CH[-c(1:2),]
      
    df3_CH<-rbind(header,df2_CH)
  
    
    


#Engage Pilot Numeric

df_Num <- read.csv("Engage_Pilot_Numeric.csv", header=FALSE,na.strings="")

x <- df_Num[2,]
colnames(df_Num) <- x

df2_Num<-df_Num[-c(2:3),]
df2_Num<-df2_Num[-c(1:17)]


y<-df2_Num[146]
y<-na.omit(y)
y<-y[-c(1),]

library(stringr)
library(tidyverse)

y<-gsub("\\D","",y,ignore.case = TRUE, fixed =FALSE)

hours<-as.data.frame(substr(y,1,2))

hours<-hours%>%rename(Hours=`substr(y, 1, 2)`)
hours$Hours <- as.numeric(as.character(hours$Hours))



library(ggplot2)

ggplot(hours, aes(x = Hours)) +           
  geom_histogram(alpha = 0.5, position = "identity") +
  geom_vline(aes(xintercept=mean(hours, na.rm=T)),   # Ignore NA values for mean
             color="blue", linetype="dotted", size=1) +
  labs(x="Average number of hours worked (per week)")
