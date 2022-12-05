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
  
  subpacket<-data.frame(Type="literal_value",Value=strtoi(code,base=2),Value2=substr(packet,1, 11+n*5),total_length_subpackets="x",number_subpackets="x",Length=11+n*5,Version=version,ID=ID, packet=substr(packet, 12+n*5, nchar(packet)))
  return(subpacket)
}
