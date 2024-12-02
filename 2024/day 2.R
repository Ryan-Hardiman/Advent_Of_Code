source("get_aoc.R")
library(tidyverse)
data<-get_aoc(2)
data<- data|>str_extract_all("\\d+")|>lapply(as.numeric)

check <- function(x){
  all(head(x,-1)-tail(x,-1)<0 & head(x,-1)-tail(x,-1)> -4 )|
    all(head(x,-1)-tail(x,-1)>0 & head(x,-1)-tail(x,-1)< 4 )
}

#Part1
map(data, check)|>unlist()|>sum()

remove_1 <- function(x){
  out<-list()
  for(i in 1:length(x)) out<-c(out,list(x[-i]))
  return(out)
}
#part2
map(data,~.x|>remove_1()|>map(\(y)check(y))|>unlist()|>any())|>unlist()|>sum()
