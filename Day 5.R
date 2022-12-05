setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)

data<-read.csv("lines.csv")
data2<-data %>% filter(y1==y2|x1==x2)

df<-data.frame()
df[1:1000,1:1000]<-0
for (i in 1:1000){
  data3<-filter(data2,x1==x2&x2==i)
  if (nrow(data3!=0)) { for (j in 1:nrow(data3)) { df[(data3$y1[j]):(data3$y2[j]),i]<-df[(data3$y1[j]):(data3$y2[j]),i]+1 }}
  data4<-filter(data2,y1==y2&y2==i)
  if (nrow(data4!=0)) { for (j in 1:nrow(data4)) { df[i,(data4$x1[j]):(data4$x2)[j]]<-df[i,(data4$x1[j]):(data4$x2[j])]+1 }}
}
sum(df>1)

##############################################################################################################################

data_diagonal<-anti_join(data,data2)

for (i in 1:nrow(data_diagonal)){
df[(data_diagonal$y1[i]),(data_diagonal$x1[i])]<-df[(data_diagonal$y1[i]),(data_diagonal$x1[i])]+1

if ((data_diagonal$x2[i])>(data_diagonal$x1[i]) & (data_diagonal$y2[i])>(data_diagonal$y1[i])){
  length<-(data_diagonal$x2[i])-(data_diagonal$x1[i])
  for (j in 1:length) { df[(data_diagonal$y1[i]+j),(data_diagonal$x1[i])+j]<-df[(data_diagonal$y1[i]+j),(data_diagonal$x1[i]+j)]+1 }
}

if ((data_diagonal$x2[i])>(data_diagonal$x1[i]) & (data_diagonal$y2[i])<(data_diagonal$y1[i])){
  length<-(data_diagonal$x2[i])-(data_diagonal$x1[i])
  for (j in 1:length) { df[(data_diagonal$y1[i]-j),(data_diagonal$x1[i])+j]<-df[(data_diagonal$y1[i]-j),(data_diagonal$x1[i]+j)]+1 }
}

if ((data_diagonal$x2[i])<(data_diagonal$x1[i]) & (data_diagonal$y2[i])<(data_diagonal$y1[i])){
  length<-(data_diagonal$x1[i])-(data_diagonal$x2[i])
  for (j in 1:length) { df[(data_diagonal$y1[i]-j),(data_diagonal$x1[i])-j]<-df[(data_diagonal$y1[i]-j),(data_diagonal$x1[i]-j)]+1 }
}

if ((data_diagonal$x2[i])<(data_diagonal$x1[i]) & (data_diagonal$y2[i])>(data_diagonal$y1[i])){
  length<-(data_diagonal$x1[i])-(data_diagonal$x2[i])
  for (j in 1:length) { df[(data_diagonal$y1[i]+j),(data_diagonal$x1[i])-j]<-df[(data_diagonal$y1[i]+j),(data_diagonal$x1[i]-j)]+1 }
}

}
sum(df>1)













