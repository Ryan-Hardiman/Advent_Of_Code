source("get_aoc.R")
library(tidyverse)
data<-get_aoc(2)|>str_extract_all("\\d+")|>lapply(as.numeric)

check <- function(x){
  all((head(x,-1)-tail(x,-1)) %in% -3:-1)|
    all((head(x,-1)-tail(x,-1))%in% 1:3)
}

#Part1 390
map(data, check)|>unlist()|>sum()

remove_1 <- function(x){
  map(1:length(x),  ~x[-.x]|>check())|>unlist()|>any()
}
#part2 439
map(data,remove_1)|>unlist()|>sum()
