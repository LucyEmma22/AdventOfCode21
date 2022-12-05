setwd("~/OneDrive - University of Edinburgh/Advent of Code")

player_1<-data.frame(position=6,score=0, dice=0)
player_2<-data.frame(position=7,score=0, dice=0)
turns<-seq(0,1000,2)

for (i in turns){
  dice_roll<-6+9*i
  position<-player_1$position[nrow(player_1)]+dice_roll
  if (position>10){position<-as.numeric(substr(as.character(position),nchar(as.character(position)),nchar(as.character(position))))}
  if (position==0){position<-10}
  score<-player_1$score[nrow(player_1)]+position
  player_1<-rbind(player_1,data.frame(position=position,score=score,dice=dice_roll))

  if (player_1$score[nrow(player_1)]>=1000){
    print("player 1 wins")
    losing_score<-player_2$score[nrow(player_2)]
    break}
  
  dice_roll_2<-6+9*(i+1)
  position<-player_2$position[nrow(player_2)]+dice_roll_2
  if (position>10){position<-as.numeric(substr(as.character(position),nchar(as.character(position)),nchar(as.character(position))))}
  if (position==0){position<-10}
  score<-player_2$score[nrow(player_2)]+position
  player_2<-rbind(player_2,data.frame(position=position,score=score,dice=dice_roll))
  
  if (player_2$score[nrow(player_2)]>=1000){
    print("player 2 wins")
    losing_score<-player_1$score[nrow(player_1)]
    break}
  
}

((nrow(player_1)+nrow(player_2)-2)*3)*losing_score

#############################################################################################################################################################################################
data<-read.csv("list.csv",header=FALSE)
