source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(1,2016)

parse_data <-function(x){
  x|>str_extract_all("[LR]\\d+")|>
    lapply(str_replace_all,"R","-1i")|>
    lapply(str_replace_all,"L","1i")|>
    lapply(str_extract_all,"^.+i|\\d+")|>
    unlist(recursive = F)|>
    lapply(as.complex)
}

part_1 <-function(x){
  out <-reduce(parse_data(x), \(acc,nxt) {
    acc[[1]] <- acc[[1]]*nxt[[1]]
    acc[[2]] <- acc[[2]] + acc[[1]]*nxt[[2]]
    return(acc)},
    .init = as.complex(c("1i","0")))
  
  abs(Re(out[[2]])+Im(out[[2]]))
}


c_range <- function(x,y){
  pmap_vec(list(Re(x):Re(y),Im(x):Im(y)),
       \(x,y) as.complex(paste0(x,if(y>=0){"+"},y,"i")))%>%
    tail(-1)
}

part_2 <-function(x){
  out <-reduce(parse_data(x), \(acc,nxt) { 
    if(any(duplicated(acc[[3]])))return(acc)
    start<-acc[[2]]
    acc[[1]] <- acc[[1]]*nxt[[1]]
    acc[[2]] <- acc[[2]] + acc[[1]]*nxt[[2]]
    acc[[3]] <- c(acc[[3]],c_range(start,acc[[2]]))
    acc},
    .init = as.complex(c("1i","0","0"))|>as.list())
  out <- out[[3]][which(duplicated(out[[3]]))]
  abs(Re(out)+Im(out))
}

#Answers:
part_1(data) #246
part_2(data) #124