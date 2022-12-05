setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)
library(stringr)

data<-read.csv("cave.csv",header=FALSE)
all_cave_names<-unique(c("start",unlist(data[data!="start"&data!="end"]),"end"))
all_caves<-list()
for (i in 1:length(all_cave_names)){
  cave<-data.frame(X=filter(data,V1==all_cave_names[i]|V2==all_cave_names[i])[filter(data,V1==all_cave_names[i]|V2==all_cave_names[i])!=all_cave_names[i]])
  colnames(cave)<-all_cave_names[i]
  assign(all_cave_names[i],cave)
  all_caves[[i]]<-cave
}


start_path<-list("start")
   
all_paths2<-list()
   
   for (j in 1:length(start_path)){
  path<-start_path[[j]]
  
if(last(path)=="end"){cave<-end}
  else{cave<-all_caves[[match(last(path),all_cave_names)]]}
  
  if(nrow(cave)!=0){
    all_paths<-list()
    for (i in 1:nrow(cave)){
    next_cave<-cave[i,1]
    lower_path<-path[str_detect(path,"[[:lower:]]")==TRUE]
    lower_duplicates<-lower_path[!duplicated(lower_path)]
    if(next_cave=="start"|(next_cave %in% path==TRUE & (str_detect(next_cave,"[[:lower:]]"))==TRUE & length(lower_path)!=length(lower_duplicates))){
      all_paths[[i]]<-"Invalid"
    }else if (colnames(cave)=="end"){
      all_paths[[i]]<-path
    }else{
      all_paths[[i]]<-c(path,next_cave)

    }
  }
  }
  all_paths2[[j]]<-all_paths
   }
   
  start_path<-do.call(c, all_paths2)
  start_path<-unique(start_path[start_path!="Invalid"])
  length(start_path)
