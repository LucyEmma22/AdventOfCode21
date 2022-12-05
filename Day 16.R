setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(stringr)
library(dplyr)

options(scipen=999)
packet<-"820D4A801EE00720190CA005201682A00498014C04BBB01186C040A200EC66006900C44802BA280104021B30070A4016980044C800B84B5F13BFF007081800FE97FDF830401BF4A6E239A009CCE22E53DC9429C170013A8C01E87D102399803F1120B4632004261045183F303E4017DE002F3292CB04DE86E6E7E54100366A5490698023400ABCC59E262CFD31DDD1E8C0228D938872A472E471FC80082950220096E55EF0012882529182D180293139E3AC9A00A080391563B4121007223C4A8B3279B2AA80450DE4B72A9248864EAB1802940095CDE0FA4DAA5E76C4E30EBE18021401B88002170BA0A43000043E27462829318F83B00593225F10267FAEDD2E56B0323005E55EE6830C013B00464592458E52D1DF3F97720110258DAC0161007A084228B0200DC568FB14D40129F33968891005FBC00E7CAEDD25B12E692A7409003B392EA3497716ED2CFF39FC42B8E593CC015B00525754B7DFA67699296DD018802839E35956397449D66997F2013C3803760004262C4288B40008747E8E114672564E5002256F6CC3D7726006125A6593A671A48043DC00A4A6A5B9EAC1F352DCF560A9385BEED29A8311802B37BE635F54F004A5C1A5C1C40279FDD7B7BC4126ED8A4A368994B530833D7A439AA1E9009D4200C4178FF0880010E8431F62C880370F63E44B9D1E200ADAC01091029FC7CB26BD25710052384097004677679159C02D9C9465C7B92CFACD91227F7CD678D12C2A402C24BF37E9DE15A36E8026200F4668AF170401A8BD05A242009692BFC708A4BDCFCC8A4AC3931EAEBB3D314C35900477A0094F36CF354EE0CCC01B985A932D993D87E2017CE5AB6A84C96C265FA750BA4E6A52521C300467033401595D8BCC2818029C00AA4A4FBE6F8CB31CAE7D1CDDAE2E9006FD600AC9ED666A6293FAFF699FC168001FE9DC5BE3B2A6B3EED060"
conversion_chart<-read.csv("conversion_chart.csv",header=TRUE,colClasses = "character")
all_packets<-data.frame()

for (i in 1:nrow(conversion_chart)){
  packet<-str_replace_all(packet, conversion_chart$from[i], conversion_chart$to[i])
}

######################################################################################################################################################

full_binary_conversion<-function(code){
  df<-data.frame(binary=str_split(code,""),power=2^(seq(nchar(code)-1,0)))
  colnames(df)<-c("binary","power")
  df$convertion<-ifelse(df$binary=="1",df$power,0)
  converted<-sum(df$convertion)
  return(converted)
}


operator_number <- function(packet) {
  number_subpackets<-strtoi(substr(packet, 8, 18),base=2)
  subpacket<-data.frame(Type="operator",Value="x",total_length_subpackets="x",number_subpackets=number_subpackets,Length=18,Version=version,ID=ID,  packet=substr(packet, 19, nchar(packet)))
  return(subpacket)
}

operator_length <- function(packet) {
  total_length_subpackets<-strtoi(substr(packet, 8, 22),base=2)
  subpacket<-data.frame(Type="operator",Value="x",total_length_subpackets=total_length_subpackets,number_subpackets="x",Length=22,Version=version,ID=ID,packet=substr(packet, 23, nchar(packet)))
  return(subpacket)
}

literal_value <- function(packet) {
  code<-vector()
  for (n in 0:100){
    if (substr(packet, 7+n*5, 7+n*5)=="1"){
      code<-paste(code,substr(packet, 8+n*5, 11+n*5),sep="")
    }
    if (substr(packet, 7+n*5, 7+n*5)=="0"){
      code<-paste(code,substr(packet, 8+n*5, 11+n*5),sep="")
      break
    }
  }
  subpacket<-data.frame(Type="literal_value",Value=full_binary_conversion(code),total_length_subpackets="x",number_subpackets="x",Length=11+n*5,Version=version,ID=ID, packet=substr(packet, 12+n*5, nchar(packet)))
  return(subpacket)
}


for (k in 1:10000){
  
  version<-strtoi(substr(packet, 1, 3),base=2)
  ID<-strtoi(substr(packet, 4, 6),base=2)
  
  if (ID==4){
    subpacket<-literal_value(packet)
    all_packets<-rbind(all_packets,subpacket)
    packet<-subpacket$packet
  
  }else if (ID!=4 & substr(packet, 7, 7)=="1"){
    subpacket<-operator_number(packet)
    all_packets<-rbind(all_packets,subpacket)
    packet<-subpacket$packet
    
  }else if (substr(packet, 7, 7)=="0"){
    subpacket<-operator_length(packet)
    all_packets<-rbind(all_packets,subpacket)
    packet<-subpacket$packet
  }
  
}

sum(all_packets$Version)

###############################################################################################################################################################################################################

all_packets2<-all_packets
all_packets<-all_packets2

all_packets$Value<-as.numeric(all_packets$Value)
all_packets$Value<-ifelse(is.na(all_packets$Value),0,all_packets$Value)

for (k in 1:100000){
  for (i in 1: nrow(all_packets)){
    
    
    if (all_packets$Type[i]=="literal_value"){
      
      
      if(all_packets$Length[i]==all_packets$total_length_subpackets[i-1]|all_packets$number_subpackets[i-1]==1){
        all_packets$Length[i]<-all_packets$Length[i]+all_packets$Length[i-1]
        all_packets<-all_packets[-(i-1),]
        break
      }
      
      
      if (all_packets$Type[i+1]=="literal_value"){
        
        
        if((all_packets$Length[i]+all_packets$Length[i+1]==all_packets$total_length_subpackets[i-1]) | (all_packets$number_subpackets[i-1]==2)){
          all_packets$Length[i]<-all_packets$Length[i]+all_packets$Length[i+1]+all_packets$Length[i-1]
          if (all_packets$ID[i-1]==0){all_packets$Value[i]<-all_packets$Value[i]+all_packets$Value[i+1]}
          if (all_packets$ID[i-1]==1){all_packets$Value[i]<-all_packets$Value[i]*all_packets$Value[i+1]}
          if (all_packets$ID[i-1]==2){all_packets$Value[i]<-min(all_packets$Value[i],all_packets$Value[i+1])}
          if (all_packets$ID[i-1]==3){all_packets$Value[i]<-max(all_packets$Value[i],all_packets$Value[i+1])}
          if (all_packets$ID[i-1]==5){all_packets$Value[i]<-ifelse(all_packets$Value[i]>all_packets$Value[i+1],1,0)}
          if (all_packets$ID[i-1]==6){all_packets$Value[i]<-ifelse(all_packets$Value[i]<all_packets$Value[i+1],1,0)}
          if (all_packets$ID[i-1]==7){all_packets$Value[i]<-ifelse(all_packets$Value[i]==all_packets$Value[i+1],1,0)}
          all_packets<-all_packets[-c((i-1),(i+1)),]
          break
        }
        
        
        if ((all_packets$Type[i+2])=="literal_value"){
          
          
          if((all_packets$Length[i]+all_packets$Length[i+1]+all_packets$Length[i+2]==all_packets$total_length_subpackets[i-1]) | (all_packets$number_subpackets[i-1]==3)){
            all_packets$Length[i]<-all_packets$Length[i]+all_packets$Length[i+1]+all_packets$Length[i+2]+all_packets$Length[i-1]
            if (all_packets$ID[i-1]==0){all_packets$Value[i]<-all_packets$Value[i]+all_packets$Value[i+1]+all_packets$Value[i+2]}
            if (all_packets$ID[i-1]==1){all_packets$Value[i]<-all_packets$Value[i]*all_packets$Value[i+1]*all_packets$Value[i+2]}
            if (all_packets$ID[i-1]==2){all_packets$Value[i]<-min(all_packets$Value[i],all_packets$Value[i+1],all_packets$Value[i+2])}
            if (all_packets$ID[i-1]==3){all_packets$Value[i]<-max(all_packets$Value[i],all_packets$Value[i+1],all_packets$Value[i+2])}
            all_packets<-all_packets[-c((i-1),(i+1),(i+2)),]
            break
          }
          
          
          if ((all_packets$Type[i+3])=="literal_value"){
            
            
            if((all_packets$Length[i]+all_packets$Length[i+1]+all_packets$Length[i+2]+all_packets$Length[i+3]==all_packets$total_length_subpackets[i-1]) | (all_packets$number_subpackets[i-1]==4)){
              all_packets$Length[i]<-all_packets$Length[i]+all_packets$Length[i+1]+all_packets$Length[i+2]+all_packets$Length[i+3]+all_packets$Length[i-1]
              if (all_packets$ID[i-1]==0){all_packets$Value[i]<-all_packets$Value[i]+all_packets$Value[i+1]+all_packets$Value[i+2]+all_packets$Value[i+3]}
              if (all_packets$ID[i-1]==1){all_packets$Value[i]<-all_packets$Value[i]*all_packets$Value[i+1]*all_packets$Value[i+2]*all_packets$Value[i+3]}
              if (all_packets$ID[i-1]==2){all_packets$Value[i]<-min(all_packets$Value[i],all_packets$Value[i+1],all_packets$Value[i+2],all_packets$Value[i+3])}
              if (all_packets$ID[i-1]==3){all_packets$Value[i]<-max(all_packets$Value[i],all_packets$Value[i+1],all_packets$Value[i+2],all_packets$Value[i+3])}
              all_packets<-all_packets[-c((i-1),(i+1),(i+2),(i+3)),]
              break
            }
            
            
            if ((all_packets$Type[i+4])=="literal_value"){
              
              
              if((all_packets$Length[i]+all_packets$Length[i+1]+all_packets$Length[i+2]+all_packets$Length[i+3]+all_packets$Length[i+4]==all_packets$total_length_subpackets[i-1]) | (all_packets$number_subpackets[i-1]==5)){
                all_packets$Length[i]<-all_packets$Length[i]+all_packets$Length[i+1]+all_packets$Length[i+2]+all_packets$Length[i+3]+all_packets$Length[i+4]+all_packets$Length[i-1]
                if (all_packets$ID[i-1]==0){all_packets$Value[i]<-all_packets$Value[i]+all_packets$Value[i+1]+all_packets$Value[i+2]+all_packets$Value[i+3]+all_packets$Value[i+4]}
                if (all_packets$ID[i-1]==1){all_packets$Value[i]<-all_packets$Value[i]*all_packets$Value[i+1]*all_packets$Value[i+2]*all_packets$Value[i+3]*all_packets$Value[i+4]}
                if (all_packets$ID[i-1]==2){all_packets$Value[i]<-min(all_packets$Value[i],all_packets$Value[i+1],all_packets$Value[i+2],all_packets$Value[i+3],all_packets$Value[i+4])}
                if (all_packets$ID[i-1]==3){all_packets$Value[i]<-max(all_packets$Value[i],all_packets$Value[i+1],all_packets$Value[i+2],all_packets$Value[i+3],all_packets$Value[i+4])}
                all_packets<-all_packets[-c((i-1),(i+1),(i+2),(i+3),(i+4)),]
                break
              }
              
              
            }}}}}}
  if (nrow(filter(all_packets,Type!="literal_value"))==1){break}
}

sum(all_packets$Value)


