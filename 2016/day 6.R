library(tidyverse)
data <- get_aoc(2016,4)

parse_data <- function(x,fn){
  x|>str_extract_all(".")|>
    lapply(map, \(x) str_count(x,letters))|>
    reduce(\(acc,nxt) map2(acc,nxt,`+`))|>
    map_chr(~letters[which(.x == do.call(fn,list(.x)))])|>
    glue::glue_collapse()
}

part_1 <- function(x){
  x|>parse_data("max")
}

part_2 <- function(x){
  x|>parse_data("min")
}

#Answers:
part_1(data) #wkbvmikb
part_2(data) #evakwaga


