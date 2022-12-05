setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)

data<-read.csv("depth measurements.csv")


data1<-data
data1$difference<-ifelse(data1$measurement>data1$prior_measurement,"increased","decreased")
data1<-filter(data1,difference=="increased")
nrow(data1)


data2<-select(data,measurement)
for (i in 1:length(data2$measurement)){
  data2$sum[i]<-data2$measurement[i]+data2$measurement[i+1]+data2$measurement[i+2]
}
data2$sum2<-lag(data2$sum)
data2$difference<-ifelse(data2$sum>data2$sum2,"increased","decreased")
data2<-filter(data2,difference=="increased")
nrow(data2)
