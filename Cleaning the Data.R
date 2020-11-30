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
df2_Num<-df2_Num[-c(1),-c(1:17)]



# Hours 
y<-df2_Num[146]
y<-na.omit(y)
y<-y[-c(1),]


library(stringr)
library(tidyverse)
library(lavaan)
library(sem)
library(semPlot)

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

Con1<-df2_Num[,1:36]
Con1<-na.omit(Con1)
Con2<-df2_Num[,37:72]
Con3<-df2_Num[,73:108]
Con4<-df2_Num[,109:144]


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


Con1<-Con1%>%rename(C1.1a=`C1.1 - I enjoy thinking about work even when Iâ€™m not at work.`,
                    C1.1b=`C1.1 - Most days, I feel happiest when the workday is soon to be complete.`,
                    C1.1c=`C1.1 - I am happiest when I am immersed in a project.`,
                    C1.1d=`C1.1 - I love starting my workday.`,
                    
                    C1.2a=`C1.2 - I enjoy spending time completing my job tasks.`,
                    C1.2b=`C1.2 - Most days I feel enthusiastic about starting my work day.`,
                    C1.2c=`C1.2 - I feel motivated to go beyond what is asked of me.`,
                    C1.2d=`C1.2 - This job drains my energy.`,
                    
                    C1.3a=`C1.3 - I am proud to be a member of this organization.`,
                    C1.3b=`C1.3 - I feel supported by my supervisor when I fail at a task.`,
                    C1.3c=`C1.3 - I feel proud of my accomplishments within this organization.`,
                    C1.3d=`C1.3 - My job makes me feel like Iâ€™m part of something meaningful.`,
                    
                    C1.4a=`C1.4 - I devote more time than is expected of me.`,
                    C1.4b=`C1.4 - I have to be reminded to take breaks while Iâ€™m at work.`,
                    C1.4c=`C1.4 - I never miss a work deadline.`,
                    C1.4d=`C1.4 - I never allow distractions to interfere with my work.`,
                    
                    C1.5a=`C1.5 - When work is slow I find ways to be productive.`,
                    C1.5b=`C1.5 - I express enthusiasm for my job while at work.`,
                    C1.5c=`C1.5 - I try my best to perform well at work.`,
                    C1.5d=`C1.5 - If I notice my energy level is low, I take corrective steps to re-energize.`,
                    
                    C1.6a=`C1.6 - I make valued contributions to the organization.`,
                    C1.6b=`C1.6 - I embrace challenging situations at work.`,
                    C1.6c=`C1.6 - I speak positively about this organization to others.`,
                    C1.6d=`C1.6 - This organization provides the resources necessary for me to successfully perform my job.`,
                    
                    C1.7a=`C1.7 - I devote my full attention to my work tasks throughout the day.`,
                    C1.7b=`C1.7 - Thinking about work saps my energy.`,
                    C1.7c=`C1.7 - I would rather direct my focus toward a work task than a personal task.`,
                    C1.7d=`C1.7 - Iâ€™m able to maintain good levels of energy throughout the workday.`,
                    
                    C1.8a=`C1.8 - I plan my future with this company.`,
                    C1.8b=`C1.8 - I believe this company cares about my career goals.`,
                    C1.8c=`C1.8 - I often think about finding another job.`,
                    C1.8d=`C1.8 - This organization challenges me to work at my full potential.`,
                    
                    C1.9a=`C1.9 - Iâ€™m able to concentrate on my work without distractions.`,
                    C1.9b=`C1.9 - I have a hard time detaching mentally from my work.`,
                    C1.9c=`C1.9 - Time passes quickly while Iâ€™m working.`,
                    C1.9d=`C1.9 - I find it difficult to mentally disconnect from work.`
                    )





# CFA Models that work

Model1.1<-'
Affective =~ C1.1a + C1.1b + C1.1c + C1.1d + C1.2a + C1.2b + C1.2c + C1.2d + C1.3a + C1.3b + C1.3c + C1.3d
Behavioral =~ C1.4a + C1.4b + C1.4c + C1.4d + C1.5a + C1.5b + C1.5c + C1.5d + C1.6a + C1.6b + C1.6c + C1.6d
Cognitive =~ C1.9a + C1.9b + C1.9c + C1.9d + C1.7a + C1.7b + C1.7c + C1.7d + C1.8a + C1.8b + C1.8c + C1.8d
'
Fit1.1<-lavaan::cfa(Model1.1, data = Con1)
semPlot::semPaths(Fit1.1,whatLabels = "std", layout = "tree")
lavaan::fitMeasures(Fit1.1)
summary(Fit1.1, fit.measure=TRUE)



Model1.2<-'
Affective =~ C1.1a + C1.1b + C1.1c + C1.1d + C1.2a + C1.2b + C1.2c + C1.2d + C1.3a + C1.3b + C1.3c + C1.3d
Behavioral =~ C1.4a + C1.4b + C1.4c + C1.4d + C1.5a + C1.5b + C1.5c + C1.5d + C1.6a + C1.6b + C1.6c + C1.6d
Cognitive =~ C1.9a + C1.9b + C1.9c + C1.9d + C1.7a + C1.7b + C1.7c + C1.7d + C1.8a + C1.8b + C1.8c + C1.8d
global =~ Affective + Behavioral + Cognitive
'
Fit1.2<-lavaan::cfa(Model1.2, data = Con1)
semPlot::semPaths(Fit1.2,whatLabels = "std", layout = "tree")
lavaan::fitMeasures(Fit1.2)
summary(Fit1.2, fit.measure=TRUE)


ModelBifactor<-'
Affective =~ C1.1a + C1.1b + C1.1c + C1.1d + C1.2a + C1.2b + C1.2c + C1.2d + C1.3a + C1.3b + C1.3c + C1.3d
Behavioral =~ C1.4a + C1.4b + C1.4c + C1.4d + C1.5a + C1.5b + C1.5c + C1.5d + C1.6a + C1.6b + C1.6c + C1.6d
Cognitive =~ C1.9a + C1.9b + C1.9c + C1.9d + C1.7a + C1.7b + C1.7c + C1.7d + C1.8a + C1.8b + C1.8c + C1.8d
global =~ C1.1a + C1.1b + C1.1c + C1.1d + C1.2a + C1.2b + C1.2c + C1.2d + C1.3a + C1.3b + C1.3c + C1.3d + C1.4a + C1.4b + C1.4c + C1.4d + C1.5a + C1.5b + C1.5c + C1.5d + C1.6a + C1.6b + C1.6c + C1.6d + C1.9a + C1.9b + C1.9c + C1.9d + C1.7a + C1.7b + C1.7c + C1.7d + C1.8a + C1.8b + C1.8c + C1.8d
'

FitBifactor<-lavaan::cfa(ModelBifactor, data = Con1, orthogonal=TRUE, std.lv=TRUE)
semPlot::semPaths(FitBifactor,whatLabels = "std", layout = "tree")
lavaan::fitMeasures(FitBifactor)
summary(FitBifactor, fit.measure=TRUE)



# CFA that do not work


first.model = '
Affective.Absorption =~ C1.1a + C1.1b + C1.1c + C1.1d
Affective.Vigor =~ C1.2a + C1.2b + C1.2c + C1.2d
Affective.Dedication =~ C1.3a + C1.3b + C1.3c + C1.3d
Behavioral.Absorption =~ C1.4a + C1.4b + C1.4c + C1.4d
Behavioral.Vigor =~ C1.5a + C1.5b + C1.5c + C1.5d
Behavioral.Dedication =~ C1.6a + C1.6b + C1.6c + C1.6d
Cognitive.Absorption =~ C1.9a + C1.9b + C1.9c + C1.9d
Cognitive.Vigor =~ C1.7a + C1.7b + C1.7c + C1.7d
Cogntive.Dedication =~ C1.8a + C1.8b + C1.8c + C1.8d
'


second.model ='
Affective.Absorption =~ C1.1a + C1.1b + C1.1c + C1.1d
Affective.Vigor =~ C1.2a + C1.2b + C1.2c + C1.2d
Affective.Dedication =~ C1.3a + C1.3b + C1.3c + C1.3d
Behavioral.Absorption =~ C1.4a + C1.4b + C1.4c + C1.4d
Behavioral.Vigor =~ C1.5a + C1.5b + C1.5c + C1.5d
Behavioral.Dedication =~ C1.6a + C1.6b + C1.6c + C1.6d
Cognitive.Absorption =~ C1.9a + C1.9b + C1.9c + C1.9d
Cognitive.Vigor =~ C1.7a + C1.7b + C1.7c + C1.7d
Cogntive.Dedication =~ C1.8a + C1.8b + C1.8c + C1.8d
generalfactor =~ Affective.Absorption + Affective.Vigor + Affective.Dedication + Behavioral.Absorption + Behavioral.Vigor + Behavioral.Dedication + Cognitive.Absorption + Cognitive.Vigor + Cogntive.Dedication
'

alternative.model ='
Affective.Absorption =~ C1.1a + C1.1b + C1.1c + C1.1d
Affective.Vigor =~ C1.2a + C1.2b + C1.2c + C1.2d
Affective.Dedication =~ C1.3a + C1.3b + C1.3c + C1.3d
Behavioral.Absorption =~ C1.4a + C1.4b + C1.4c + C1.4d
Behavioral.Vigor =~ C1.5a + C1.5b + C1.5c + C1.5d
Behavioral.Dedication =~ C1.6a + C1.6b + C1.6c + C1.6d
Cognitive.Absorption =~ C1.9a + C1.9b + C1.9c + C1.9d
Cognitive.Vigor =~ C1.7a + C1.7b + C1.7c + C1.7d
Cogntive.Dedication =~ C1.8a + C1.8b + C1.8c + C1.8d
generalfactor =~ NA*Affective.Absorption + Affective.Vigor + Affective.Dedication + Behavioral.Absorption + Behavioral.Vigor + Behavioral.Dedication + Cognitive.Absorption + Cognitive.Vigor + Cogntive.Dedication
generalfactor ~~ 1*generalfactor
'


bifactor.model='
Affective.Absorption =~ C1.1a + C1.1b + C1.1c + C1.1d
Affective.Vigor =~ C1.2a + C1.2b + C1.2c + C1.2d
Affective.Dedication =~ C1.3a + C1.3b + C1.3c + C1.3d
Behavioral.Absorption =~ C1.4a + C1.4b + C1.4c + C1.4d
Behavioral.Vigor =~ C1.5a + C1.5b + C1.5c + C1.5d
Behavioral.Dedication =~ C1.6a + C1.6b + C1.6c + C1.6d
Cognitive.Absorption =~ C1.9a + C1.9b + C1.9c + C1.9d
Cognitive.Vigor =~ C1.7a + C1.7b + C1.7c + C1.7d
Cogntive.Dedication =~ C1.8a + C1.8b + C1.8c + C1.8d
generalfactor =~ C1.1a + C1.1b + C1.1c + C1.1d + C1.2a + C1.2b + C1.2c + C1.2d + C1.3a + C1.3b + C1.3c + C1.3d + C1.4a + C1.4b + C1.4c + C1.4d + C1.5a + C1.5b + C1.5c + C1.5d + C1.6a + C1.6b + C1.6c + C1.6d + C1.9a + C1.9b + C1.9c + C1.9d + C1.7a + C1.7b + C1.7c + C1.7d + C1.8a + C1.8b + C1.8c + C1.8d
'




