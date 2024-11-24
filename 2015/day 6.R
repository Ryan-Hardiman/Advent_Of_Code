library(tidyverse)
data <- clipr::read_clip()|>str_extract_all("toggle|on|off|\\d+")
xy <- data |>lapply(tail,4)|>lapply(as.numeric)
action <- data|>lapply(head,-4)

grid <- array(-1,c(1000,1000))

part1 <- function(grid, xy,action){
  grid[xy[[1]]:xy[[3]]+1,xy[[2]]:xy[[4]]+1] <- if(action=="toggle"){(grid[xy[[1]]:xy[[3]]+1,xy[[2]]:xy[[4]]+1])*(-1)}else if(
    action == "off"){-1}else if(action == "on"){1}
  grid
}


answer_1 <- (reduce2(xy,action,part1,.init = grid)+1)|>sum()/2
answer_1

part2 <- function(grid, xy,action){
  grid[xy[[1]]:xy[[3]]+1,xy[[2]]:xy[[4]]+1] <- if(action=="toggle"){(grid[xy[[1]]:xy[[3]]+1,xy[[2]]:xy[[4]]+1])+2}else if(
    action == "off"){pmax(grid[xy[[1]]:xy[[3]]+1,xy[[2]]:xy[[4]]+1]-1,0)}else if(action == "on"){grid[xy[[1]]:xy[[3]]+1,xy[[2]]:xy[[4]]+1]+1}
  grid
}
grid <- array(0,c(1000,1000))
answer_1 <- reduce2(xy,action,part2,.init = grid)
answer_1|>sum()
