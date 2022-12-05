setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)
library(stringr)
library(tidyr)
library(modeest)
  

data<-read.table("input.txt",colClasses = "character" )
#options(scipen=999)
data<-data.frame(str_split_fixed(data$V1, "", 12))

#data <- data.frame(sapply( data, as.numeric ))
#colSums(data,na.rm=TRUE)/(1000-colSums(is.na(data)))-0.5

all_mode<-vector()
for (i in 1:ncol(data)){
  data2<-select(data,i)
  mode<-mfv(data2[,1])
  all_mode[i]<-mode
}

strtoi("001100010000", base = 2)
strtoi("110011101111", base = 2)

##########################################################################################

data<-read.table("input.txt",colClasses = "character" )
data<-data.frame(str_split_fixed(data$V1, "", 12))
for (i in 1:ncol(data)){
  mode<-as.numeric(mfv(data[,i]))
  if (length(mode)==2){mode<-1}
    data<-data %>% filter(data[,i] == mode)
    if(nrow(data)==1){data2<-data}
}
strtoi("001100001101", base = 2)
781


data<-read.table("input.txt",colClasses = "character" )
data<-data.frame(str_split_fixed(data$V1, "", 12))
for (i in 1:ncol(data)){
  mode<-as.numeric(mfv(data[,i]))
  if (length(mode)==2){mode<-1}
  mode<-1-mode
  data<-data %>% filter(data[,i] == mode)
  if(nrow(data)==1){data2<-data}
}
strtoi("101010101110", base = 2)
2734