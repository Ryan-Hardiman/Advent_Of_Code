source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(05,2025)

part_1 <- function(x){
  ranges <- x[grepl("-",x)]
  ingreds <-x[!grepl("-",x)]|>tail(-1)|>as.numeric()
  starts <- map_chr(ranges, str_extract, "^\\d+")|>as.numeric()
  ends <- map_chr(ranges, str_extract,"\\d+$")|>as.numeric() 
  map_lgl(ingreds, ~any(.x>=starts & .x<=ends))|>sum() 
  }

part_2 <- function(x){
  ranges <- x[grepl("-", x)]
  starts <- map_chr(ranges, str_extract, "^\\d+") |> as.numeric()
  ends   <- map_chr(ranges, str_extract, "\\d+$") |> as.numeric()
  
  df <- tibble(start = starts, end = ends) |> arrange(start)
  
  merged <- list()
  cur_start <- df$start[1]
  cur_end   <- df$end[1]
  
  for(i in 2:nrow(df)){
    s <- df$start[i]
    e <- df$end[i]
    
    if(s <= cur_end + 1){
      cur_end <- max(cur_end, e)
    } else {
      merged <- append(merged, list(c(cur_start, cur_end)))
      cur_start <- s
      cur_end   <- e
    }
  }
  
  merged <- append(merged, list(c(cur_start, cur_end)))
  
  sum(map_dbl(merged, ~ (.x[2] - .x[1] + 1)))
}

#Answers:
part_1(data) # 744
part_2(data) # 347468726696961
