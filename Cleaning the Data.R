# Engage Pilot Choice Text
df_CH <- read.csv(file="E:/Montclair State University/Engage_Pilot_Choice.csv", header=FALSE,,na.strings="")


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

df_Num <- read.csv(file="E:/Montclair State University/Engage_Pilot_Numeric.csv", header=FALSE,,na.strings="")

df2_Num<-df_Num[-c(3),]
df2_Num<-df2_Num[-c(1:17)]


df2_Num$V163[df2_Num$V163=="10-16"] <-13

