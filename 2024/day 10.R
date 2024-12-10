library(tidyverse) 
library(magrittr)
data <- get_aoc(10,2024) 

#Detail 
#We use matrix and which(.. , arr.ind = TRUE) to get the complex 
# representation of digits in a map. "Next_index" returns the complex value of 
# each of the next integers in the grid. We then map over our current "heads" 
# taking the absolute value of the head to our possible new head positions. 
# For Part 1 we only want the unique end positions per head, and so we unique it 
# For Part 2 this is removed. 
next_index <- function(nxt) {
  out <- which(mat == nxt, arr.ind = TRUE)
  out[, 1] + out[, 2] * 1i
}

trailhead <- function(acc, nxt, part_2 = FALSE) {
  nxt <- next_index(nxt)
  map(acc, ~ nxt[which(abs(.x - nxt) == 1)])|>unlist()%>%`if`(part_2,.,unique(.))
}

walk_trails <- function(data, part_2 = FALSE) {
  len <- nchar(data[[1]])
  mat <<- data |> str_extract_all(".")|>unlist()|>as.numeric()|>matrix(ncol =len) #Cheeky double insert
  starts <- which(mat == 0, arr.ind = TRUE)
  starts <- starts[, 1] + starts[, 2] * 1i
  map_dbl(starts,  ~ reduce(1:9, trailhead, part_2, .init = .x)|>length())|>sum()
}

part_1 <- function(x) {
  walk_trails(x)
}
part_2 <- function(x) {
  walk_trails(x, TRUE)
}

part_1(data) #552
part_2(data) #1255
