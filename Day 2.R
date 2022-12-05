setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)

data<-read.csv("directions.csv")
forward<-filter(data,direction=="forward")
total_forward<-sum(forward$value)

up<-filter(data,direction=="up")
total_up<-sum(up$value)

down<-filter(data,direction=="down")
total_down<-sum(down$value)

total<-total_forward*(total_down-total_up)


data<-read.csv("directions.csv")
data$value<-ifelse(data$direction=="up",-data$value,data$value)
data$direction<-ifelse(data$direction=="up","down",data$direction)
forward<-0
depth<-0
aim<-0
for (i in 1:1000){
  if (data$direction[i]=="forward"){
    forward<-forward+data$value[i]
    depth<-depth+data$value[i]*aim
  } else {
    aim<-aim+data$value[i]
  }
}
total<-depth*forward

down X:  aim = aim + X 
up X: aim = aim - X 
forward X: forward = forward + X , depth = aim*X

