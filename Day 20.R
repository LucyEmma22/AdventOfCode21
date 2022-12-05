setwd("~/OneDrive - University of Edinburgh/Advent of Code")

data<-read.csv("image_input_example.csv",header=FALSE)
algorithm<-("..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#")
data<-read.csv("image_input.csv",header=FALSE)
algorithm<-"###.#..##.####.....#.#..##.#....##.##.##..#.####.#.#.#.#.#..##.####.####...##.#.###..#..#.##...##..##..#....###########..####..###....#.##...##...#....#########.#..#..##..#..#.#.#.##...###..####.##.#####.####.#....#.#.##..###...#..#...##.##.#..#...#..##..#..#...###........#..##....#.#.##.##.##.######..##.#...#..#######.###.#..#.#....#.###.#....#.....#.#..##.#.......##..#..#..#...#..##.######.#.####.#....#.....#.##.#.#..##.#..##..#.##..#.##...###......##.#####..##.###.#.#.######.#####..#.#..#.....#....#.##.."

algorithm<-unlist(strsplit(algorithm,""))
data<-t(data.frame(strsplit(data$V1, "")))
rownames(data)<-NULL
output_image<-data

for (i in 1:25){
data<-output_image
extras<-2
data[data=="."]<-0
data[data=="#"]<-1
extra_left_right<-data.frame(matrix("0",nrow(data),extras))
data<-cbind(extra_left_right,data)
data<-cbind(data,extra_left_right)
colnames(data)<-1:ncol(data)
extra_top_bottom<-data.frame(matrix("0",extras,ncol(data)))
colnames(extra_top_bottom)<-1:ncol(data)
data<-rbind(extra_top_bottom,data)
data<-rbind(data,extra_top_bottom)

output_image<-data.frame(matrix(0,nrow(data)-2,ncol(data)-2))
for (y in 2:(nrow(data)-1)){
  for (x in 2:(ncol(data)-1)){
    number<-c(data[(y-1),(x-1)], data[(y-1),(x)], data[(y-1),(x+1)], data[(y),(x-1)], data[(y),(x)], data[(y),(x+1)], data[(y+1),(x-1)], data[(y+1),(x)], data[(y+1),(x+1)])
    output_image[y-1,x-1]<-algorithm[(strtoi(paste(number, collapse = ''),base=2))+1]
  }
}


  data<-output_image
  extras<-2
  data[data=="."]<-0
  data[data=="#"]<-1
  extra_left_right<-data.frame(matrix("1",nrow(data),extras))
  data<-cbind(extra_left_right,data)
  data<-cbind(data,extra_left_right)
  colnames(data)<-1:ncol(data)
  extra_top_bottom<-data.frame(matrix("1",extras,ncol(data)))
  colnames(extra_top_bottom)<-1:ncol(data)
  data<-rbind(extra_top_bottom,data)
  data<-rbind(data,extra_top_bottom)
  
  output_image<-data.frame(matrix(0,nrow(data)-2,ncol(data)-2))
  for (y in 2:(nrow(data)-1)){
    for (x in 2:(ncol(data)-1)){
      number<-c(data[(y-1),(x-1)], data[(y-1),(x)], data[(y-1),(x+1)], data[(y),(x-1)], data[(y),(x)], data[(y),(x+1)], data[(y+1),(x-1)], data[(y+1),(x)], data[(y+1),(x+1)])
      output_image[y-1,x-1]<-algorithm[(strtoi(paste(number, collapse = ''),base=2))+1]
    }
  }
}

sum(output_image=="#")