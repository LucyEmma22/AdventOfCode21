setwd("~/OneDrive - University of Edinburgh/Advent of Code")
library(dplyr)

# target area: x=14..50, y=-267..-225
# target area: x=20..30, y=-10..-5

t_xmin<-14
t_xmax<-50
t_ymin<-(-267)
t_ymax<-(-225)

all<-data.frame()
for (j in 1:1000){

  for (k in -1000:1000){

    x_pos<-0
    y_pos<-0
    x_vel<-j
    y_vel<-k
    trajectory<-data.frame()
    
    for (i in 1:1000){ 
      
      x_pos<-x_pos+x_vel
      y_pos<-y_pos+y_vel
     
      if (x_vel>0){x_vel<-x_vel-1
      }else if (x_vel<0){x_vel<-x_vel+1
      }else if (x_vel==0){x_vel<-x_vel
      }
      
      y_vel<-y_vel-1
      trajectory<-rbind(trajectory,c(x_pos,y_pos))
      
      if(t_xmin <= x_pos & x_pos <= t_xmax & (t_ymax) >= y_pos & y_pos >= (t_ymin)){
        all<-rbind(all,data.frame(result=max(trajectory[2]),x=j,y=k))
        break}
      if(x_pos > t_xmax | y_pos < t_ymin){break}
    }
    
  }
}
unique(filter(all,result==max(result))$result)
nrow(all)
