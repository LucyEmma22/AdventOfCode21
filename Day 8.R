setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)
library(stringr)

data<-read.csv("digit_output.csv",header=FALSE)
all_counts<-vector()
for (i in 1:4){
counts<-str_count(data[,i])
all_counts<-c(all_counts,counts)}
seven<-sum(all_counts==3)
four<-sum(all_counts==4)
one<-sum(all_counts==2)
eight<-sum(all_counts==7)
sum(all_counts==3)+sum(all_counts==4)+sum(all_counts==2)+sum(all_counts==7)

############################################################################################################################################
data2<-read.csv("digits.csv",header=FALSE)
data<-read.csv("digit_output.csv",header=FALSE)

data2<-data.frame(t(data2))
data<-data.frame(t(data))
list<-paste0("X",c(1:200))

total_output<-0
for (j in 1:200){
data3<-select(data2,list[j])
colnames(data3)<-"X1"
data3$count<-str_count(data3$X1)
data3<-arrange(data3,count)

input<-select(data,list[j])
colnames(input)<-"X1"
input$count<-str_count(input$X1)

data3<-rbind(data3,input)

two<-unlist(str_split(data3[1,1],""))
seven<-unlist(str_split(data3[2,1],""))
four<-unlist(str_split(data3[3,1],""))
for (i in 1:14){
  data3$number[i]<-NA
if (data3$count[i]==2) {data3$number[i]<-1
}else if(data3$count[i]==3){data3$number[i]<-7
}else if(data3$count[i]==4){data3$number[i]<-4
}else if(data3$count[i]==7){data3$number[i]<-8
}else if (data3$count[i]==5 & sum(str_count(data3$X1[i], pattern=seven))==3){data3$number[i]<-3
}else if (data3$count[i]==5 & sum(str_count(data3$X1[i], pattern=four))==2){data3$number[i]<-2
}else if (data3$count[i]==5 & is.na(data3$number[i])){data3$number[i]<-5
}else if (data3$count[i]==6 & sum(str_count(data3$X1[i], pattern=four))==4){data3$number[i]<-9
}else if (data3$count[i]==6 & sum(str_count(data3$X1[i], pattern=two))==1){data3$number[i]<-6
}else if (data3$count[i]==6 & is.na(data3$number[i])){data3$number[i]<-0
}
}
output<-data3[11:14,3]
output<-output[1]*1000+output[2]*100+output[3]*10+output[4]
total_output<-total_output+output
}
