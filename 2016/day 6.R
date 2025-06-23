library(tidyverse)
data <- get_aoc(2016,4)

parse_data <- function(x){
  x|>str_extract_all(".")|>
    lapply(map, \(x) str_count(x,letters))
}


get_counts <- function(x){
  reduce(x, \(acc,nxt){
         map2(acc,nxt, `+`)})
}


return_most_common <- function(x){
  map_chr(x, \(x) letters[which(x == max(x))])|>
    glue::glue_collapse()
}

return_least_common <- function(x){
  map_chr(x, \(x) letters[which(x == min(x))])|>
    glue::glue_collapse()
}


part_1 <- function(x){
  x|>parse_data()|>
    get_counts()|>
    return_most_common()
}

part_2 <- function(x){
  x|>parse_data()|>
    get_counts()|>
    return_least_common()
}



#Answers:
part_1(data) #wkbvmikb
part_2(data) #evakwaga

