source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(07,2025)

#Make each line a Boolean vec. S required for initial state.
parse_data <- function(x){
  x%>%str_split("")%>%map(`%in%`,c("S","^"))
}


do_next <- function(acc,nxt){
  
  #Part 2 introduces timelines, we store the number of timelines in the name of
  # acc[[1]] This avoids needing to do possible 2^n work..
  timelines <- names(acc[[1]])%>%as.numeric()
  
  to_split <-acc[[1]] & nxt 
  to_fall_through <- acc[[1]] & !nxt
  split <- lag(to_split,default = F) | lead(to_split,default = F)
  #Part 1 Score is current score + number of splits.
  acc[[2]] <- acc[[2]] + sum(to_split) 
  #Propagate any new split beam or ones that weren't split.
  acc[[1]] <- split|to_fall_through 
  
  
  #Main body of logic for part 2, temporarily collect the values that are split.
  tmp <- timelines * to_split
  #Add them to the next row, for left / right, and remove them from the next row in place.
  timelines <- timelines + lag(tmp,default = 0) + lead(tmp,default = 0) - timelines*to_split
  #naming must come last
  acc[[1]] <- set_names(acc[[1]], timelines)
  
  acc
}





part_1 <-function(x){
lines <- parse_data(x)
reduce(tail(lines,-1),
       do_next,
       .init = list(
         set_names(lines[[1]],1*lines[[1]]) #Bit wasteful here.. 
         ,0))[[2]]
}


part_2 <-function(x){
  lines <- parse_data(x)
  out<-reduce(tail(lines,-1),
         do_next,
         .init = list(
           set_names(lines[[1]],1*lines[[1]]),0))
  #Collect timelines and sum them.
  out[[1]]%>%names()%>%as.numeric()%>%sum()
}

#Answers:
part_1(data) # 1642
part_2(data) # 47274292756692
