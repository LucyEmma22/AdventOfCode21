operator_number <- function(packet) {
  
  number_subpackets<-strtoi(substr(packet, 8, 18),base=2)
  
  subpacket<-data.frame(Type="operator",Value="x",Value2="x",total_length_subpackets="x",number_subpackets=number_subpackets,Length=18,Version=version,ID=ID,  packet=substr(packet, 19, nchar(packet)))
  
  return(subpacket)
}


