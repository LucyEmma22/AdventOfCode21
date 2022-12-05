# cave_function <- function(start_path) {
#   
#   cave<-all_caves[[match(last(start_path),all_cave_names)]]
#   for (i in 1:nrow(cave)){
#     path<-start_path
#     next_cave<-all_caves[[match(cave[i,1],all_cave_names)]]
#     
#     if(colnames(next_cave)=="start"|(colnames(next_cave) %in% path==TRUE & str_detect(colnames(next_cave),"[[:lower:]]")==TRUE)){
#       all_paths[[i]]<-"Invalid"
#     }else if (colnames(next_cave)=="end"){
#       all_paths[[i]]<-c(start_path,"end")
#     }else{
#       all_paths[[i]]<-c(start_path,colnames(next_cave))
# 
#     }
#   }
#   
#   return(c(all_paths))
# }


cave_function <- function(start_path) {
  all_paths<-data.frame()
  cave<-all_caves[[match(last(start_path),all_cave_names)]]
  for (i in 1:nrow(cave)){
    path<-start_path
    next_cave<-all_caves[[match(cave[i,1],all_cave_names)]]
    
    if(colnames(next_cave)=="start"|(colnames(next_cave) %in% path==TRUE & str_detect(colnames(next_cave),"[[:lower:]]")==TRUE)){
      path<-data.frame(start_path,"Invalid")
      all_paths<-rbind(all_paths,path)
    }else if (colnames(cave)=="end"){
      path<-data.frame(start_path,"Finished")
      all_paths<-rbind(all_paths,path)
    }else{
      path<-data.frame(start_path,colnames(next_cave))
      all_paths<-rbind(all_paths,path)
    }
  }
  
  return(all_paths)
}