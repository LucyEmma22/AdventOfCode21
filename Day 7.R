setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)

data<-read.csv("crabs.csv",header=FALSE)

df2<-data.frame()
for (i in 0:max(data)){
  total_h_distance<-0
  for (j in 1:nrow(data)){
    h_distance<-abs(data[j,1]-i)
    total_h_distance<-total_h_distance+h_distance
  }
  df<-data.frame(total_h_distance=total_h_distance,h_position=i)
  df2<-rbind(df2,df)
}

##########################################################################################################################################################

df2<-data.frame()
for (i in 0:max(data)){
  total_fuel<-0
  for (j in 1:nrow(data)){
    h_distance<-abs(data[j,1]-i)
    fuel<-((h_distance*(h_distance+1))/2)
    total_fuel<-total_fuel+fuel
  }
  df<-data.frame(total_fuel=total_fuel,h_position=i)
  df2<-rbind(df2,df)
}


