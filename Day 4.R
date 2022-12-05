setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)
library(stringr)
library(tidyr)


data<-na.omit(read.csv("bingo boards.csv", header = FALSE))
data$board<-rep(c(1:(500/5)), each = 5)

# for (i in seq(0,594,6)){
#   assign(paste0("board",i/6),data[(i+1):(i+5),])
# }
WINNER<-"none"
for (i in c(23,30,70,61,79,49,19,37,64,48,72,34,69,53,15,74,89,38,46,36,28,32,45,2,39,58,11,62,97,40,14,87,96,94,91,92,80,99,6,31,57,98,65,10,33,63,42,17,47,66,26,22,73,27,7,0,55,8,56,29,86,25,4,12,51,60,35,50,5,75,95,44,16,93,21,3,24,52,77,76,43,41,9,84,67,71,83,88,59,68,85,82,1,18,13,78,20,90,81,54)){
  data$V1<-ifelse(data$V1==i,NA,data$V1)
  data$V2<-ifelse(data$V2==i,NA,data$V2)
  data$V3<-ifelse(data$V3==i,NA,data$V3)
  data$V4<-ifelse(data$V4==i,NA,data$V4)
  data$V5<-ifelse(data$V5==i,NA,data$V5)
  for(j in 1:(500/5)){
    board<-filter(data,board==j)
    if(nrow(filter_all(board[,1:5],any_vars(!is.na(.))))!=5){WINNER<-board}
    if(ncol(select_if(board,function(x) any(!is.na(x))))!=6){WINNER<-board}
  }
  if (WINNER!="none"){print(WINNER)
    break}
}

colSums(WINNER,na.rm=TRUE)

#######################################################################################################################################

data<-na.omit(read.csv("bingo boards.csv", header = FALSE))
data$board<-rep(c(1:(500/5)), each = 5)
for (i in c(23,30,70,61,79,49,19,37,64,48,72,34,69,53,15,74,89,38,46,36,28,32,45,2,39,58,11,62,97,40,14,87,96,94,91,92,80,99,6,31,57,98,65,10,33,63,42,17,47,66,26,22,73,27,7,0,55,8,56,29,86,25,4,12,51,60,35,50,5,75,95,44,16,93,21,3,24,52,77,76,43,41,9,84,67,71,83,88,59,68,85,82,1,18,13,78,20,90,81,54)){
  data$V1<-ifelse(data$V1==i,NA,data$V1)
  data$V2<-ifelse(data$V2==i,NA,data$V2)
  data$V3<-ifelse(data$V3==i,NA,data$V3)
  data$V4<-ifelse(data$V4==i,NA,data$V4)
  data$V5<-ifelse(data$V5==i,NA,data$V5)
  for(j in 1:(500/5)){
    board<-filter(data,board==j)
    if(nrow(filter_all(board[,1:5],any_vars(!is.na(.))))!=5){data<-filter(data,board!=j)}
    if(ncol(select_if(board,function(x) any(!is.na(x))))!=6){data<-filter(data,board!=j)}
  }
if (nrow(data)==5){print(data)}
  if (nrow(data)==0){break}
}

(81+78+90+1+13+18)*82


