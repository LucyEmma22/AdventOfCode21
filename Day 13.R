setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)

data<-read.csv("origami.csv")

grid<-matrix(0,max(data$y)+1,max(data$x)+1)
for (i in 1:nrow(data)){
grid[data$y[i]+1,data$x[i]+1]<-1
}

all_folds<-data.frame(direction=c("x","y","x","y","x","y","x","y","x","y","y","y"),position=c(655,447,327,223,163,111,81,55,40,27,13,6))

for (i in 1:nrow(all_folds)){
  fold<-all_folds[i,]

if(fold$direction=="y"){
### fold along y ###
yfold<-fold$position
top<-grid[1:(yfold),]
bottom<-grid[(yfold+2):nrow(grid),]
bottom<-bottom[nrow(bottom):1,]

if (nrow(top)<nrow(bottom)){
  extra<-matrix(0,nrow(bottom)-nrow(top),ncol(top))
  top<-rbind(extra,top)
} 

if (nrow(bottom)<nrow(top)){
  extra<-matrix(0,nrow(top)-nrow(bottom),ncol(bottom))
  bottom<-rbind(extra,bottom)
}
grid<-bottom+top
}

### fold along x ###
if (fold$direction=="x"){
  xfold<-fold$position
  right<-grid[,(xfold+2):ncol(grid)]
  left<-grid[,1:xfold]
  right<-right[,ncol(right):1]
  
  if (ncol(right)<ncol(left)){
    extra<-matrix(0,nrow(right),nrow(left)-nrow(right))
    right<-cbind(extra,right)
  } 
  
  if (nrow(left)<nrow(right)){
    extra<-matrix(0,nrow(left),nrow(right)-nrow(left))
    left<-rbind(extra,left)
  }
  grid<-left+right
}
}


sum(grid!=0)