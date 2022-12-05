setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(stringr)
data<-read.table("snail_numbers.txt")

#data<-data.frame(V1=c("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]","[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]","[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]","[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]","[7,[5,[[3,8],[1,4]]]]","[[2,[2,2]],[8,[8,1]]]","[2,9]","[1,[[[9,3],9],[[9,0],[0,7]]]]","[[[5,[7,4]],7],1]","[[[[4,2],2],6],[8,7]]"))
#data<-data.frame(V1=c("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]","[[[5,[2,8]],4],[5,[[9,9],0]]]","[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]","[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]","[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]","[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]","[[[[5,4],[7,7]],8],[[8,3],8]]","[[9,3],[[9,9],[6,[4,9]]]]","[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]","[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"))

data$V1<-strsplit(data$V1, "")
addition<-unlist(data$V1[1])

for (x in 2:nrow(data)){
  addition<-c("[",addition,",",unlist(data$V1[x]),"]")
  
  for (n in 1:1000){
    
    ### EXPLODE ###
    
    nested<-0
    count1<-count2<-0
    for (i in 1:length(addition)){
      if (addition[i]=="["){
        count1<-count1+1
      }else if (addition[i]=="]"){
          count2<-count2+1}
      if (count1-count2>=5 & addition[i]=="[" & addition[i+2]=="," & addition[i+4]=="]"){
        nested<-i
        break}
      }
    
    if(as.numeric(nested)!=0){
      for (k in 5:(length(addition)-nested)){
        if (is.na(as.numeric(addition[nested+k]))!=TRUE){addition[nested+k]<-as.character(as.numeric(addition[nested+k])+as.numeric(addition[nested+3]))
        break}
        }
      for (k in 1:(nested-1)){
        if (is.na(as.numeric(addition[nested-k]))!=TRUE){addition[nested-k]<-as.character(as.numeric(addition[nested-k])+as.numeric(addition[nested+1]))
        break}
        }
      addition<-addition[-(nested+1):-(nested+4)]
      addition[nested]<-0
      
      ### SPLIT ###
      
      } else if (sum(na.omit(as.numeric(addition))>9)!=0){
        for (m in 1:length(addition)){
          if (is.na(as.numeric(addition[m]))==FALSE & as.numeric(addition[m])>=10){
            addition<-append(addition, c("[",floor(as.numeric(addition[m])/2),",",ceiling(as.numeric(addition[m])/2),"]"), after=m)
            addition<-addition[-m]
            break
          }
        }
        
      } else {break}
    
  }
  }

paste(addition, collapse = '')


for (j in 1:1000){
for (i in 1:length(addition)){
if (addition[i]=="[" & addition[i+2]=="," & addition[i+4]=="]"){
  x<-as.numeric(addition[i+1])*3+as.numeric(addition[i+3])*2
  addition<-append(addition, x, after=(i+4))
  addition<-addition[-i:-(i+4)]
  break
}
}
  
}
paste(addition)


#####################################################################################################################################################################

data<-read.table("snail_numbers.txt")

#data<-data.frame(V1=c("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]","[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]","[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]","[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]","[7,[5,[[3,8],[1,4]]]]","[[2,[2,2]],[8,[8,1]]]","[2,9]","[1,[[[9,3],9],[[9,0],[0,7]]]]","[[[5,[7,4]],7],1]","[[[[4,2],2],6],[8,7]]"))
#data<-data.frame(V1=c("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]","[[[5,[2,8]],4],[5,[[9,9],0]]]","[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]","[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]","[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]","[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]","[[[[5,4],[7,7]],8],[[8,3],8]]","[[9,3],[[9,9],[6,[4,9]]]]","[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]","[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"))

data$V1<-strsplit(data$V1, "")

all<-data.frame()

for (p in 1:nrow(data)){
  addition1<-unlist(data$V1[p])
  
  for (y in 1:nrow(data)){ 
  addition<-c("[",addition1,",",unlist(data$V1[y]),"]")

  
  for (n in 1:1000){
    
    ### EXPLODE ###
    
    nested<-0
    count1<-count2<-0
    for (i in 1:length(addition)){
      if (addition[i]=="["){
        count1<-count1+1
      }else if (addition[i]=="]"){
        count2<-count2+1}
      if (count1-count2>=5 & addition[i]=="[" & addition[i+2]=="," & addition[i+4]=="]"){
        nested<-i
        break}
    }
    
    if(as.numeric(nested)!=0){
      for (k in 5:(length(addition)-nested)){
        if (is.na(as.numeric(addition[nested+k]))!=TRUE){addition[nested+k]<-as.character(as.numeric(addition[nested+k])+as.numeric(addition[nested+3]))
        break}
      }
      for (k in 1:(nested-1)){
        if (is.na(as.numeric(addition[nested-k]))!=TRUE){addition[nested-k]<-as.character(as.numeric(addition[nested-k])+as.numeric(addition[nested+1]))
        break}
      }
      addition<-addition[-(nested+1):-(nested+4)]
      addition[nested]<-0
      
      ### SPLIT ###
      
    } else if (sum(na.omit(as.numeric(addition))>9)!=0){
      for (m in 1:length(addition)){
        if (is.na(as.numeric(addition[m]))==FALSE & as.numeric(addition[m])>=10){
          addition<-append(addition, c("[",floor(as.numeric(addition[m])/2),",",ceiling(as.numeric(addition[m])/2),"]"), after=m)
          addition<-addition[-m]
          break
        }
      }
      
    } else {break}
    
  }
  
  for (j in 1:1000){
    for (i in 1:length(addition)){
      if (addition[i]=="[" & addition[i+2]=="," & addition[i+4]=="]"){
        x<-as.numeric(addition[i+1])*3+as.numeric(addition[i+3])*2
        addition<-append(addition, x, after=(i+4))
        addition<-addition[-i:-(i+4)]
        break
      }
    }
    
  }
  df<-data.frame(result=addition,first_number=p,second_number=y)
  all<-rbind(all,df)
  paste(addition)
  }
}

all$result2<-as.numeric(all$result)
max(all$result2)
