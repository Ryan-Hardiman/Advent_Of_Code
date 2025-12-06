source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(04,2025)

tst <- clipr::read_clip()


parse_complex_grid <- function(x) {
  chars <- do.call(rbind, strsplit(x, "", fixed = TRUE))
  
  nr <- nrow(chars)
  nc <- ncol(chars)
  
  re <- rep(seq_len(nc), each = nr)
  im <- rep(rev(seq_len(nr)), times = nc)
  
  vals <- as.vector(chars)
  names(vals) <- paste0(re, "+", im, "i")
  
  vals == "@"
}

eight_offsets <- c(
  1, -1, 1i, -1i,
  1+1i, 1-1i, -1+1i, -1-1i
)

count_adjacent_8 <- function(z, grid) {
  neigh <- z + eight_offsets
  vals <- grid[paste0(neigh)]
  sum(vals,na.rm = T)
}


consume <- function(curr, score){
  if(length(setdiff(curr,score)) ==0) return(score)
  pos <- names(curr)%>%as.complex()
  counts <- sapply(pos, count_adjacent_8,grid= curr)
  new_score <- score + sum(counts <4)
  new_curr <- curr[-which(counts<4)]
  Tailcall(consume,new_curr,new_score)
}

part_1 <- function(x) {
  grid <- parse_complex_grid(x)
  rolls <- names(grid)[grid] |> as.complex()
  neighbour_counts <- sapply(rolls, count_adjacent_8, grid = grid)
  sum(neighbour_counts < 4)
}


part_2 <- function(x) {
  grid <- parse_complex_grid(x)
  grid <- grid[grid]
  consume(grid,0)
}


#Answers:
part_1(data) # 1370
part_2(data) # 8437
