source("get_aoc.R")
library(tidyverse)
data<-get_aoc(1)|>str_extract_all("\\d+")
first <-data|>lapply(pluck,1)|>unlist()|>as.numeric()
last <-data|>lapply(pluck,2)|>unlist()|>as.numeric()

#part1 1580061
abs(sort(first)-sort(last))|>sum()
#part2 23046913
map(first, \(x) x*length(which(last==x)))|>unlist()|>sum()

