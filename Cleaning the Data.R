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

df_Num <- read.csv("Qualtrics_pilot_data.csv", header=TRUE,na.strings="")

df2_Num<-df_Num[-c(1:3),]

df2_Num<-df2_Num%>%select(C1.1_1:Q29)




library(stringr)
library(tidyverse)
library(lavaan)
library(sem)
library(semPlot)
library(ggplot2)
library(carData)



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


# Creating Condition Column - Option 1

Condition1<-as.data.frame(ifelse(df2_Num%>%select(C1.1_1:C1.9_4)!='NA',"Condition 1"))

Condition1<-Condition1%>%unite(Condition, C1.1_1, C1.1_2, C1.1_3 , C1.1_4 , C1.2_1 , C1.2_2 , C1.2_3 , 
                                      C1.2_4 , C1.3_1 , C1.3_2 , C1.3_3 , C1.3_4,
                                      C1.4_1 , C1.4_2 , C1.4_3 , C1.4_4 , 
                                      C1.5_1 , C1.5_2 , C1.5_3 , C1.5_4 , 
                                      C1.6_1 , C1.6_2 , C1.6_3 , C1.6_4,
                                      C1.7_1 , C1.7_2 , C1.7_3 , C1.7_4 , 
                                      C1.8_1 , C1.8_2 , C1.8_3 , C1.8_4,
                                      C1.9_1 , C1.9_2 , C1.9_3 , C1.9_4, na.rm=TRUE
)

Condition1<-as.data.frame(substr(Condition1$Condition,1,11))
Condition1<-Condition1%>%rename(Condition_1=`substr(Condition1$Condition, 1, 11)`)

#condition2

Condition2<-as.data.frame(ifelse(df2_Num%>%select(C2.1_1:C2.9_4)!='NA',"Condition 2"))

Condition2<-Condition2%>%unite(Condition, C2.1_1, C2.1_2, C2.1_3 , C2.1_4 , C2.2_1 , C2.2_2 , C2.2_3 , 
                               C2.2_4 , C2.3_1 , C2.3_2 , C2.3_3 , C2.3_4,
                               C2.4_1 , C2.4_2 , C2.4_3 , C2.4_4 , 
                               C2.5_1 , C2.5_2 , C2.5_3 , C2.5_4 , 
                               C2.6_1 , C2.6_2 , C2.6_3 , C2.6_4,
                               C2.7_1 , C2.7_2 , C2.7_3 , C2.7_4 , 
                               C2.8_1 , C2.8_2 , C2.8_3 , C2.8_4,
                               C2.9_1 , C2.9_2 , C2.9_3 , C2.9_4, na.rm=TRUE
)

Condition2<-as.data.frame(substr(Condition2$Condition,1,11))
Condition2<-Condition2%>%rename(Condition_2=`substr(Condition2$Condition, 1, 11)`)

#Condition 3
Condition3<-as.data.frame(ifelse(df2_Num%>%select(C3.1_1:C3.3_12)!='NA',"Condition 3"))

Condition3<-Condition3%>%unite(Condition, C3.1_1, C3.1_2, C3.1_3 , C3.1_4 , C3.1_5 , C3.1_6 , C3.1_7 , 
                               C3.1_8 , C3.1_9 , C3.1_10 , C3.1_11 , C3.1_12,
                               C3.2_1 , C3.2_2 , C3.2_3 , C3.2_4 , 
                               C3.2_5 , C3.2_6 , C3.2_7 , C3.2_8 , 
                               C3.2_9 , C3.2_10 , C3.2_11 , C3.2_12,
                               C3.3_1 , C3.3_2 , C3.3_3 , C3.3_4 , 
                               C3.3_5 , C3.3_6 , C3.3_7 , C3.3_8,
                               C3.3_9 , C3.3_10 , C3.3_11 , C3.3_12, na.rm=TRUE
)

Condition3<-as.data.frame(substr(Condition3$Condition,1,11))
Condition3<-Condition3%>%rename(Condition_3=`substr(Condition3$Condition, 1, 11)`)

#Condition 4


Condition4<-as.data.frame(ifelse(df2_Num%>%select(C4.1_1:C4.3_12)!='NA',"Condition 4"))

Condition4<-Condition4%>%unite(Condition, C4.1_1, C4.1_2, C4.1_3 , C4.1_4 , C4.1_5 , C4.1_6 , C4.1_7 , 
                               C4.1_8 , C4.1_9 , C4.1_10 , C4.1_11 , C4.1_12,
                               C4.2_1 , C4.2_2 , C4.2_3 , C4.2_4 , 
                               C4.2_5 , C4.2_6 , C4.2_7 , C4.2_8 , 
                               C4.2_9 , C4.2_10 , C4.2_11 , C4.2_12,
                               C4.3_1 , C4.3_2 , C4.3_3 , C4.3_4 , 
                               C4.3_5 , C4.3_6 , C4.3_7 , C4.3_8,
                               C4.3_9 , C4.3_10 , C4.3_11 , C4.3_12, na.rm=TRUE
)

Condition4<-as.data.frame(substr(Condition4$Condition,1,11))
Condition4<-Condition4%>%rename(Condition_4=`substr(Condition4$Condition, 1, 11)`)





#Merging 


Condition<-cbind(Condition1, Condition2, Condition3, Condition4)

Condition<-Condition%>%unite(Conditions, Condition_1, Condition_2, Condition_3, Condition_4,
                             na.rm=TRUE)

Condition$Conditions<-trimws(Condition$Conditions)

df3_Num<-cbind(Condition,df2_Num)



# Split the Data by Condition- Option 2

library(psych)

Con1<-df2_Num%>%select(C1.1_1:C1.9_4)
Con1<-na.omit(Con1)
Con2<-df2_Num%>%select(C2.1_1:C2.9_4)
Con2<-na.omit(Con2)
Con3<-df2_Num%>%select(C3.1_1:C3.3_12)
Con3<-na.omit(Con3)
Con4<-df2_Num%>%select(C4.1_1:C4.3_12)
Con4<-na.omit(Con4)


# Changing to numeric

Con1<-as.data.frame(apply(Con1,2, as.numeric))
Con2<-as.data.frame(apply(Con2,2, as.numeric))
Con3<-as.data.frame(apply(Con3,2, as.numeric))
Con4<-as.data.frame(apply(Con4,2, as.numeric))


# Creating the new column

Con1<-cbind(Condition="Condition 1",Con1)
Con2<-cbind(Condition="Condition 2",Con2)
Con3<-cbind(Condition="Condition 3",Con3)
Con4<-cbind(Condition="Condition 4",Con4)

data<-merge(Con1,Con2, by="Condition",all=T)
data2<-merge(Con3,Con4, by="Condition", all = T)
data3<-merge(data,data2, by="Condition", all=T)





# Demographics
Demographics<-df2_Num[,145:148]
Demographics<-Demographics%>%rename(Job_Title=Q28,
                                    Hours=Q25,
                                    Years_Worked_At_Company=Q49,
                                    Feedback=Q29)

## Hours

y<-Demographics$Hours
y<-na.omit(y)

y<-gsub("\\D","",y,ignore.case = TRUE, fixed =FALSE)

hours<-as.data.frame(substr(y,1,2))

hours<-hours%>%rename(Hours=`substr(y, 1, 2)`)
hours$Hours <- as.numeric(as.character(hours$Hours))

ggplot(hours, aes(x = Hours)) +           
  geom_histogram(alpha = 0.5, position = "identity") +
  geom_vline(aes(xintercept=mean(hours, na.rm=T)),   # Ignore NA values for mean
             color="blue", linetype="dotted", size=1) +
  labs(x="Average number of hours worked (per week)")




