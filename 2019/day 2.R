source("get_aoc.R")
library(tidyverse)

data<-get_aoc(2,2019)|>str_extract_all("\\d+")|>unlist()|>as.numeric()
#data<-clipr::read_clip()|>str_extract_all("\\d+")|>unlist()|>as.numeric()


intcode <- function(acc, ind) {
  if (acc[ind] == 99) {
    return(acc[1])
  } else if (acc[ind] == 1) {
    acc[acc[ind + 3]+1] <- acc[acc[ind + 1]+1] + acc[acc[ind + 2]+1]
  } else if (acc[ind] == 2) {
    acc[acc[ind + 3]+1] <- acc[acc[ind + 1]+1] * acc[acc[ind + 2]+1]
  }
  intcode(acc,ind+4)
}
#Part1
data[2]<-12
data[3]<-2
intcode(data,1)

replacements <- function(x,y){
  data[2]<-x
  data[3]<-y
  intcode(data,1)
}

#Part2
combs <- list(1:100,1:100)|>expand.grid()
matches<-map2(combs[,1],combs[,2],replacements)
part2<-combs[which(matches==19690720),]

100*part2[1]+part2[2]
