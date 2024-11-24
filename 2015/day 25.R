library(tidyverse)
data <- clipr::read_clip()|>str_extract_all("\\d+")|>unlist()|>as.numeric()

#We are given 111'th value as 27995004,so we could continue from there.. 

get_index <- function(row,col){

  s <- 1:(row+col)
  row_start <- (c(1:row)-1)|>sum()+1
  o <- s|>tail(-row)|>head(col-1)|>sum()+row_start
  o
  
  
}

index <- get_index(data[[1]],data[[2]])


i<-0
x<-20151125
repeat{
  i<-i+1
  if(i==index) break
  x<-(x*252533)%%33554393
}

