setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)
library(tidyr)

# data<-read.csv("cavern_risk.txt",header=FALSE,colClasses = "character" )
# data<-separate(data,V1,c(as.character(0:nchar(data[1,1]))),sep="")
# data$`0`<-NULL
# data<-lapply(data,as.numeric)
# data<-data.frame(data)
# write.csv(data,file="cavern_risk.csv")

#data<-data.frame(read.csv("cavern_risk_example.csv",header=FALSE))
data<-data.frame(read.csv("cavern_risk.csv",header=FALSE))

lowest<-data.frame(x0=NA,y0=NA,x=2,y=2,value=0)
all<-lowest

for (k in 1:100000){

a<-data.frame(x0=lowest$x,y0=lowest$y,x=lowest$x+1,y=lowest$y,value=data[lowest$x+1,lowest$y]+lowest$value)
b<-data.frame(x0=lowest$x,y0=lowest$y,x=lowest$x,y=lowest$y+1,value=data[lowest$x,lowest$y+1]+lowest$value)
c<-data.frame(x0=lowest$x,y0=lowest$y,x=lowest$x-1,y=lowest$y,value=data[lowest$x-1,lowest$y]+lowest$value)
d<-data.frame(x0=lowest$x,y0=lowest$y,x=lowest$x,y=lowest$y-1,value=data[lowest$x,lowest$y-1]+lowest$value)

potential<-na.omit(rbind(a,b,c,d))
all<-rbind(all,potential)
all<-anti_join(all,lowest)

data[lowest$x,lowest$y]<-NA
lowest<-filter(all,value==min(value))[1,]



if (lowest$x==ncol(data)-1 & lowest$y==nrow(data)-1){break}
}

############################################################################################################################################

# data<-data.frame(read.csv("cavern_risk_no_na.csv",header=FALSE))
# data1<-data
# for (i in 2:9){
#   assign(paste0("panel"),eval(as.symbol(paste0("data",(i-1))))+1)
#   panel[panel==10]<-1
#   assign(paste0("data",i),panel)}
# 
# a<-cbind(data1,data2,data3,data4,data5)
# b<-cbind(data2,data3,data4,data5,data6)
# c<-cbind(data3,data4,data5,data6,data7)
# d<-cbind(data4,data5,data6,data7,data8)
# e<-cbind(data5,data6,data7,data8,data9)
# data<-rbind(a,b,c,d,e)
# write.csv(data,file="cavern_risk_times_5.csv")

data<-data.frame(read.csv("cavern_risk_times_5.csv",header=FALSE))

lowest<-data.frame(x0=NA,y0=NA,x=2,y=2,value=0)
all<-lowest

for (k in 1:100000000){
  
  a<-data.frame(x0=lowest$x,y0=lowest$y,x=lowest$x+1,y=lowest$y,value=data[lowest$x+1,lowest$y]+lowest$value)
  b<-data.frame(x0=lowest$x,y0=lowest$y,x=lowest$x,y=lowest$y+1,value=data[lowest$x,lowest$y+1]+lowest$value)
  c<-data.frame(x0=lowest$x,y0=lowest$y,x=lowest$x-1,y=lowest$y,value=data[lowest$x-1,lowest$y]+lowest$value)
  d<-data.frame(x0=lowest$x,y0=lowest$y,x=lowest$x,y=lowest$y-1,value=data[lowest$x,lowest$y-1]+lowest$value)
  
  potential<-na.omit(rbind(a,b,c,d))
  all<-rbind(all,potential)
  all<-anti_join(all,lowest)
  
  data[lowest$x,lowest$y]<-NA
  lowest<-filter(all,value==min(value))[1,]
  
  
  
  if (lowest$x==ncol(data)-1 & lowest$y==nrow(data)-1){break}
}
