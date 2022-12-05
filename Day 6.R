setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)

data<-read.csv("lanternfish.csv",header=FALSE)
for (i in 1:80){
  data[,2]<-ifelse(data[,1]==0,6,data[,1]-1)
  number_new<-sum(data[,1]==0)
  data<-data.frame(V1=data[,2])
  new<-data.frame(V1=rep(8, each = number_new))
  if(nrow(new)!=0){data<-rbind(data,new)}  
  if (i==80){break}
}
nrow(data)

###################################################################################################################################################

data<-read.csv("lanternfish.csv",header=FALSE)
data<-data %>% group_by(V1) %>% tally() %>% as.data.frame() %>% rename("clock"=V1, "start"=n) 
fish_count<-data.frame(clock=c(8:0))
fish_count<-full_join(fish_count,data,by="clock")
fish_count[is.na(fish_count)] = 0

for (i in 1:256){
fish_count[,(2+i)]<-lag(fish_count[,(1+i)])
fish_count[1,(2+i)]<-fish_count[9,(1+i)]
fish_count[3,(2+i)]<-fish_count[3,(2+i)]+fish_count[9,(1+i)]
}
options(scipen = 100)
sum(fish_count$V258)
