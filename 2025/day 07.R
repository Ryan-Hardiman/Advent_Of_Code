source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(07,2025)

#Make each line a Boolean vec. S required for initial state.
parse_data <- function(x){
  x%>%str_split("")%>%map(`%in%`,c("S","^"))
}


do_next <- function(acc,nxt){
  to_split <-acc[[1]] & nxt 
  to_fall_through <- acc[[1]] & !nxt
  split <- lag(to_split,default = F) | lead(to_split,default = F)
  #Score is current score + number of splits.
  acc[[2]] <- acc[[2]] + sum(to_split) 
  #Propagate any new split beam or ones that weren't split.
  acc[[1]] <- split|to_fall_through 
  acc
}


part_1 <-function(x){
lines <- parse_data(x)
reduce(tail(lines,-1),
       do_next,
       .init = list(lines[[1]],0))[[2]]
}


part_2 <-function(x){
x
}

#Answers:
part_1(data) # 1642
part_2(data) #