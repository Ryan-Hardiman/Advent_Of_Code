source("get_aoc.R")
library(tidyverse)
data <- get_aoc(24,2015)|>as.numeric()

#Weight per group #520
sum(data)/3

#See we can have at most 17 numbers in our group
cumsum(data)

get_perms <- function(data, goal){
  out <- list()
  for(i in seq.int(1,17)){
    perms <- combn(data, i)
    totals <- colSums(perms)
    if(any(totals == goal)){#Break as early as possible (fewest packages)
    out <- c(out, list(perms[,which(totals == goal)]))
    break 
    }
  }
  out
}

part1 <- apply(get_perms(data,520)[[1]],2,prod)|>sort()|>pluck(1)
part1

part2 <- apply(get_perms(data,sum(data)/4)[[1]],2,prod)|>sort()|>pluck(1)
part2


