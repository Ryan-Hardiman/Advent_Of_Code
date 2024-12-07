source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(7,2024)
tst <- clipr::read_clip()

get_targets <- function(x){x|>str_extract("\\d+")|>lapply(as.numeric)}
get_nums<-function(x){x|>str_extract_all(" \\d+")|>lapply(as.numeric)}

do_math <- function(acc,nxt){
  c(acc|>lapply(`+`,nxt),acc|>lapply(`*`,nxt))
}

part_1 <-function(x){
nums <- get_nums(x)
targets <- get_targets(x)
map2_dbl(nums,targets, ~any(reduce(.x,do_math)==.y)*.y)|>sum()|>format(scientific=FALSE)
}

do_more_math <- function(acc,nxt){
  c(acc|>lapply(`+`,nxt),acc|>lapply(`*`,nxt),acc|>lapply(`paste0`,nxt)|>lapply(as.bigz))
}



part_2 <-function(x){
  nums <- get_nums(x)|>lapply(as.bigz)
  targets <- get_targets(x)
  suppressWarnings(map2_dbl(nums,targets, ~any(lapply(reduce(.x,do_more_math),"==",.y))*.y)|>sum()|>format(scientific=FALSE))
}

#Answers:
part_1(data) #
part_2(data) #
