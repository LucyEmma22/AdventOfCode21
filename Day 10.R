setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)
library(tidyr)

data<-read.table("syntax.txt",header=FALSE)
n_columns<-max(nchar(data[,1]))
data<-select(separate(data,V1,paste0("V",c(0:(n_columns+1))),sep=""),-V0)

df<-data.frame()
for (k in 1:nrow(data)){
  data1<-na.omit(t(data[k,]))
  
  i<-1
  for (j in 1:1000){
    if((data1[i]=="{" & data1[i+1]=="}")|(data1[i]=="[" & data1[i+1]=="]")|(data1[i]=="<" & data1[i+1]==">")|(data1[i]=="(" & data1[i+1]==")")){
      data1[i]<-NA
      data1[i+1]<-NA
      data1<-na.omit(data1)
      i<-1
    }else if ( (data1[i]=="{" & (data1[i+1]==")"|data1[i+1]=="]"|data1[i+1]==">")) | (data1[i]=="[" & (data1[i+1]==")"|data1[i+1]=="}"|data1[i+1]==">")) | (data1[i]=="(" & (data1[i+1]=="}"|data1[i+1]=="]"|data1[i+1]==">")) | (data1[i]=="<" & (data1[i+1]==")"|data1[i+1]=="]"|data1[i+1]=="}")) ){
      df<-rbind(df,data.frame(illegal=k,symbol=data1[i+1]))
      break
    }else{i<-i+1}
    
    if (i==nrow(data1)){break}
  }
}

df$score<-ifelse(df$symbol==")",3,NA)
df$score<-ifelse(df$symbol=="]",57,df$score)
df$score<-ifelse(df$symbol=="}",1197,df$score)
df$score<-ifelse(df$symbol==">",25137,df$score)

sum(df$score)

###############################################################################################################################################################################


incomplete<-c(1:nrow(data))[-df$illegal]
all_scores<-vector()
for (k in incomplete){
  data1<-na.omit(t(data[k,]))
  
  i<-1
  for (j in 1:1000){
    if((data1[i]=="{" & data1[i+1]=="}")|(data1[i]=="[" & data1[i+1]=="]")|(data1[i]=="<" & data1[i+1]==">")|(data1[i]=="(" & data1[i+1]==")")){
      data1[i]<-NA
      data1[i+1]<-NA
      data1<-na.omit(data1)
      i<-1

    }else{i<-i+1}
    if (i==nrow(data1)){break}
  }
  data1[data1=="("]<-1
  data1[data1=="["]<-2
  data1[data1=="{"]<-3
  data1[data1=="<"]<-4
  
  score<-0
  sequence<-c(nrow(data1):1)
  for (x in sequence){
  score<-score*5+as.numeric(data1[x])
  }
  all_scores<-c(all_scores,score)
}

median(all_scores)