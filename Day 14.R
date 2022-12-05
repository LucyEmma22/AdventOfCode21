setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)
library(tidyr)

data<-read.csv("polymer.csv",header=FALSE)
polymer<-c("K","F","F","N","F","N","N","B","C","N","O","B","C","N","P","F","V","K","C","P")

for (i in 1:10){
  y<-length(polymer)
  insertions<-0
  for (j in 1:(y-1)){
    for (i in 1:nrow(data)){
      if (paste(polymer[j+insertions],polymer[j+1+insertions], sep="")==data[i,1]){
        polymer<-append(polymer, data[i,2], after=j+insertions)
        insertions<-insertions+1
        break
      }
    }
  }
}
length(polymer)
table(polymer)

######################################################################################################################################################

data<-read.csv("polymer.csv",header=FALSE)
polymer<-c("K","F","F","N","F","N","N","B","C","N","O","B","C","N","P","F","V","K","C","P")
letters<-unique(c(unlist(data$V2),polymer))

# Dataframe of all possible 2 letter combinations
df<-data.frame()
for (i in 1:length(letters)){
  x<-data.frame(letter_1=letters[i],letter_2=letters)
  df<-rbind(df,x)
}
df$polymer_2 <- paste(df$letter_1, df$letter_2,sep="")
df<-select(df,polymer_2)

# Dataframe of all letters and their frequency
all_letters<-data.frame(polymer=letters)
letters_in_polymer<-data.frame(table(polymer))
all_letters<-full_join(all_letters,letters_in_polymer,by="polymer")
all_letters$Freq<-ifelse(is.na(all_letters$Freq),0,all_letters$Freq)

# Dataframe of 2 letter combinations in start polymer and their frequency
polymer_2<-vector()
for (i in 1:(length(polymer)-1)){
  polymer_2<-c(polymer_2,paste(polymer[i],polymer[i+1],sep=""))
  }
df2<-data.frame(table(polymer_2))

# Join all possible 2 letter combinations and start polymer 2 letter combinations
df<-full_join(df,df2,by="polymer_2")
df$Freq<-ifelse(is.na(df$Freq),0,df$Freq)

data2<-separate(data,V1,c("V3","V4","V5"),sep="",remove=FALSE)
data2$add1 <- paste(data2$V4, data2$V2,sep="")
data2$add2 <- paste(data2$V2, data2$V5,sep="")
data2<-select(data2,V1,V2,add1,add2)

options(scipen = 100)
for (k in 1:40){
polymer_freq<-filter(df,Freq>0)
for (i in 1:nrow(polymer_freq)){
  gap<-polymer_freq$polymer_2[i]
  number<-polymer_freq$Freq[i]
  for (j in 1:nrow(data2)){
    if (gap==data2$V1[j]){
      df$Freq<-ifelse(df$polymer_2==data2$add1[j],df$Freq+number,df$Freq)
      df$Freq<-ifelse(df$polymer_2==data2$add2[j],df$Freq+number,df$Freq)
      df$Freq<-ifelse(df$polymer_2==data2$V1[j],df$Freq-number,df$Freq)
      all_letters$Freq<-ifelse(all_letters$polymer==data2$V2[j],all_letters$Freq+number,all_letters$Freq)
    break
      }
  }
}
}
max(all_letters$Freq)-min(all_letters$Freq)

