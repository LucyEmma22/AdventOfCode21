setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)
library(tidyr)

data<-read.table("heightmap.txt",colClasses = "character" )
n_columns<-nchar(data[1,1])
data<-separate(data,V1,paste0("V",c(0:(n_columns+1))),sep="")
data[,1]<-seq(10,10,length=nrow(data))
data[,(n_columns+2)]<-seq(10,10,length=nrow(data))
df<-t(data.frame(seq(10,10,length=n_columns+2)))
colnames(df)<-paste0("V",c(0:(n_columns+1)))
data<-rbind(df,data)
data<-rbind(data,df)
data<-data.frame(lapply(data,as.numeric))

all_low<-data.frame()
danger<-0
for (j in 2:(ncol(data)-1)){
  for (i in 2:(nrow(data)-1)){
    if (data[i,j]<data[i,j+1] & data[i,j]<data[i+1,j] & data[i,j]<data[i,j-1] & data[i,j]<data[i-1,j]){
      low<-data.frame(i=i,j=j)
      danger<-danger+data[i,j]+1
    all_low<-rbind(all_low,low)}
  }
}
data[data=="10"]<-"EDGE"
data[data=="9"]<-"EDGE"
data[data!="EDGE"]<-"basin"

###########################################################################################################################################################

all_basin_sizes<-data.frame()
for (k in 1:nrow(all_low)){
 
  y<-all_low$i[k]
  x<-all_low$j[k]
  df<-data.frame(y=y,x=x)
  for (i in 1:100){
    length<-nrow(df)
    for (p in 1:nrow(df)){
      y<-df$y[p]
      x<-df$x[p]
    if(data[y+1,x]=="basin"){df<-rbind(df,data.frame(y=y+1,x=x))}
    if(data[y,x+1]=="basin"){df<-rbind(df,data.frame(y=y,x=x+1))}
    if(data[y-1,x]=="basin"){df<-rbind(df,data.frame(y=y-1,x=x))}
    if(data[y,x-1]=="basin"){df<-rbind(df,data.frame(y=y,x=x-1))}
    df<-distinct(df)
    }
    if(length==nrow(df)){break}
  }
  basin_size<-nrow(df)
  all_basin_sizes<-rbind(all_basin_sizes,basin_size)
}
  


