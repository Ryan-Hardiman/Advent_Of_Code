source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(3,2016)

parse_data <- function(x){
  x%>%str_extract_all("\\d+")%>%
    lapply(as.numeric)
}

one <- function(x){
  x[[1]]<sum(x[[2]],x[[3]])
}

two <- function(x){
  x[[2]]<sum(x[[1]],x[[3]])
}

thr <- function(x){
  x[[3]]<sum(x[[2]],x[[1]])
}

restructure <- function(x){
  rb_list <- do.call(rbind, x)
  t_list <- split(rb_list,col(rb_list))
  reduce(t_list,\(acc,nxt) 
         c(acc,split(nxt,ceiling(seq_along(nxt)/3))),.init = NULL )
}


part_1 <-function(x){
x%>%parse_data()%>%
    map_lgl(~all(one(.x),two(.x),thr(.x)))%>%
    sum()
}


part_2 <-function(x){
  x%>%parse_data()%>%
    restructure()%>%
    map_lgl(~all(one(.x),two(.x),thr(.x)))%>%
    sum()
}

#Answers:
part_1(data) #917

part_2(data) #1649
