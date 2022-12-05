operator_length <- function(packet) {
  
  total_length_subpackets<-strtoi(substr(packet, 8, 22),base=2)
  
  subpacket<-data.frame(Type="operator",Value="x",Value2="x",total_length_subpackets=total_length_subpackets,number_subpackets="x",Length=22,Version=version,ID=ID,packet=substr(packet, 23, nchar(packet)))

  return(subpacket)
}
