setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)

data<-read.csv("octopus.csv",header=FALSE)
flashes<-0

for (j in 1:100){
  data<-data+1
  for (i in 1:20){
    for (x in 2:11){
      for (y in 2:11){
        if (data[y,x]==10){
          data[y,x]<-0
          flashes<-flashes+1
          if(data[y+1,x]>0 & data[y+1,x]<10) {data[y+1,x]<-data[y+1,x]+1}
          if(data[y+1,x+1]>0 & data[y+1,x+1]<10) {data[y+1,x+1]<-data[y+1,x+1]+1}
          if(data[y,x+1]>0 & data[y,x+1]<10) {data[y,x+1]<-data[y,x+1]+1}
          if(data[y-1,x+1]>0 & data[y-1,x+1]<10) {data[y-1,x+1]<-data[y-1,x+1]+1}
          if(data[y-1,x]>0 & data[y-1,x]<10) {data[y-1,x]<-data[y-1,x]+1}
          if(data[y-1,x-1]>0 & data[y-1,x-1]<10) {data[y-1,x-1]<-data[y-1,x-1]+1}
          if(data[y,x-1]>0 & data[y,x-1]<10) {data[y,x-1]<-data[y,x-1]+1}
          if(data[y+1,x-1]>0 & data[y+1,x-1]<10) {data[y+1,x-1]<-data[y+1,x-1]+1}
        }
      }
    }
  }
}

###############################################################################################################################################

data<-read.csv("octopus.csv",header=FALSE)
flashes<-0

for (j in 1:1000){
  if (sum(data[2:11,2:11])==0){break}
  data<-data+1
  for (i in 1:20){
    for (x in 2:11){
      for (y in 2:11){
        if (data[y,x]==10){
          data[y,x]<-0
          flashes<-flashes+1
          if(data[y+1,x]>0 & data[y+1,x]<10) {data[y+1,x]<-data[y+1,x]+1}
          if(data[y+1,x+1]>0 & data[y+1,x+1]<10) {data[y+1,x+1]<-data[y+1,x+1]+1}
          if(data[y,x+1]>0 & data[y,x+1]<10) {data[y,x+1]<-data[y,x+1]+1}
          if(data[y-1,x+1]>0 & data[y-1,x+1]<10) {data[y-1,x+1]<-data[y-1,x+1]+1}
          if(data[y-1,x]>0 & data[y-1,x]<10) {data[y-1,x]<-data[y-1,x]+1}
          if(data[y-1,x-1]>0 & data[y-1,x-1]<10) {data[y-1,x-1]<-data[y-1,x-1]+1}
          if(data[y,x-1]>0 & data[y,x-1]<10) {data[y,x-1]<-data[y,x-1]+1}
          if(data[y+1,x-1]>0 & data[y+1,x-1]<10) {data[y+1,x-1]<-data[y+1,x-1]+1}
        }
      }
    }
  }
}
