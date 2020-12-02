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

df_Num <- read.csv("Engage_Pilot_Numeric.csv", header=TRUE,na.strings="")

df2_Num<-df_Num[-c(1:3),-c(1:17)]



# Hours 
y<-df2_Num[146]
y<-na.omit(y)
y<-y[-c(1),]


library(stringr)
library(tidyverse)
library(lavaan)
library(sem)
library(semPlot)
library(ggplot2)
library(carData)

y<-gsub("\\D","",y,ignore.case = TRUE, fixed =FALSE)

hours<-as.data.frame(substr(y,1,2))

hours<-hours%>%rename(Hours=`substr(y, 1, 2)`)
hours$Hours <- as.numeric(as.character(hours$Hours))





ggplot(hours, aes(x = Hours)) +           
  geom_histogram(alpha = 0.5, position = "identity") +
  geom_vline(aes(xintercept=mean(hours, na.rm=T)),   # Ignore NA values for mean
             color="blue", linetype="dotted", size=1) +
  labs(x="Average number of hours worked (per week)")


# Reverse Items
## Condition 1
df2_Num$C1.1_2<-car::recode(df2_Num$C1.1_2,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C1.2_4<-car::recode(df2_Num$C1.2_4,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C1.7_2<-car::recode(df2_Num$C1.7_2,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C1.8_3<-car::recode(df2_Num$C1.8_3,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')

## Condition 2
df2_Num$C2.4_2<-car::recode(df2_Num$C2.4_2,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C2.3_2<-car::recode(df2_Num$C2.3_2,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C2.7_3<-car::recode(df2_Num$C2.7_3,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C2.5_4<-car::recode(df2_Num$C2.5_4,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')

## Condition 3
df2_Num$C3.1_6<-car::recode(df2_Num$C3.1_6,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C3.2_2<-car::recode(df2_Num$C3.2_2,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C3.2_8<-car::recode(df2_Num$C3.2_8,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C3.3_3<-car::recode(df2_Num$C3.3_3,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')

## Condition 4
df2_Num$C4.1_6<-car::recode(df2_Num$C4.1_6,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C4.1_11<-car::recode(df2_Num$C4.1_11,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C4.2_2<-car::recode(df2_Num$C4.2_2,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')
df2_Num$C4.2_8<-car::recode(df2_Num$C4.2_8,'1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=7')


# Split the Data by Condition

library(psych)

Con1<-df2_Num[,1:36]
Con1<-na.omit(Con1)
Con2<-df2_Num[,37:72]
Con2<-na.omit(Con2)
Con3<-df2_Num[,73:108]
Con3<-na.omit(Con3)
Con4<-df2_Num[,109:144]
Con4<-na.omit(Con4)


# Changing to numeric

Con1<-as.data.frame(apply(Con1,2, as.numeric))
Con2<-as.data.frame(apply(Con2,2, as.numeric))
Con3<-as.data.frame(apply(Con3,2, as.numeric))
Con4<-as.data.frame(apply(Con4,2, as.numeric))




# Creating Condition Column

Condition1<-as.data.frame(ifelse(df2_Num$C1.1_1!='NA',"Condition 1"))
Condition1<-Condition1%>%rename(Condition_1=`ifelse(df2_Num$C1.1_1 != "NA", "Condition 1")`)


Condition2<-as.data.frame(ifelse(df2_Num$C2.1_1!='NA',"Condition 2"))
Condition2<-Condition2%>%rename(Condition_2=`ifelse(df2_Num$C2.1_1 != "NA", "Condition 2")`)

Condition3<-as.data.frame(ifelse(df2_Num$C3.1_1!='NA',"Condition 3"))
Condition3<-Condition3%>%rename(Condition_3=`ifelse(df2_Num$C3.1_1 != "NA", "Condition 3")`)

Condition4<-as.data.frame(ifelse(df2_Num$C4.1_1!='NA',"Condition 4"))
Condition4<-Condition4%>%rename(Condition_4=`ifelse(df2_Num$C4.1_1 != "NA", "Condition 4")`)

Condition<-cbind(Condition1, Condition2, Condition3, Condition4)

Condition<-Condition%>%unite(Conditions, Condition_1, Condition_2, Condition_3, Condition_4,
                             na.rm=TRUE)

df3_Num<-cbind(Condition,df2_Num)



