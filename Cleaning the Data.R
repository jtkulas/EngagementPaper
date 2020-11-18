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



# Hours 
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



# Split the Data by Condition

library(psych)

Condition1<-df2_Num[,1:36]
Condition2<-df2_Num[,37:72]
Condition3<-df2_Num[,73:108]
Condition4<-df2_Num[,109:144]

Con1<-Condition1[-c(1),]
Con2<-Condition2[-c(1),]
Con3<-Condition3[-c(1),]
Con4<-Condition4[-c(1),]


# Changing to numeric

Con1<-as.data.frame(apply(Con1,2, as.numeric))
Con2<-as.data.frame(apply(Con2,2, as.numeric))
Con3<-as.data.frame(apply(Con3,2, as.numeric))
Con4<-as.data.frame(apply(Con4,2, as.numeric))

# Spliting Conditions Further:
  # Condition 1: affective, behavioral, cognitive
    # Affective
      
      Affective_Absorption<-Con1[,1:4]
      Affective_Vigor<-Con1[,5:8]
      Affective_Dedication<-Con1[,9:12]
    
    # Behavioral
      
      Behavioral_Absorption<-Con1[,13:16]
      Behavioral_Vigor<-Con1[,17:20]
      Behavioral_Dedication<-Con1[,21:24]
      
    # Cognitive 
      
      Cognitive_Vigor<-Con1[,25:28]
      Cognitive_Dedication<-Con1[,29:32]
      Cognitive_Absorption<-Con1[,33:36]

  # Condition 2: Absorption, Vigor, Dedication
      # Absorption
      
      Absorption_Cognitive<-Con1[,1:4]
      Absorption_Behavioral<-Con1[,5:8]
      Absorption_Affective<-Con1[,9:12]
      
      # Vigor
      
      Vigor_Cognitive<-Con1[,13:16]
      Vigor_Affective<-Con1[,17:20]
      Vigor_Behavioral<-Con1[,21:24]
      
      # Dedication
      
      Dedication_Cognitive<-Con1[,25:28]
      Dedication_Affective<-Con1[,29:32]
      Dedication_Behavioral<-Con1[,33:36]



#Items Stats
alpha(Con1,na.rm = TRUE, check.keys = TRUE)
alpha(Affective_Absorption,na.rm = TRUE,check.keys = TRUE)

warnings()

