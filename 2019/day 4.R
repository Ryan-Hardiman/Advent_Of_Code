source("get_aoc.R")
library(tidyverse)

data<-get_aoc(4,2019)|>str_extract_all("\\d+")|>unlist()|>as.numeric()

vec <- data[[1]]:data[[2]]

repeat_digit <- vec|>str_extract("(\\d)\\1")|>is.na()

check_monotonic <- function(x){
  all(paste0(x |>str_extract_all("\\d")|>unlist()|>sort(),collapse = "")|>as.numeric() ==x)
}

monotonic <- vec|>map(check_monotonic)|>unlist()
#Part1
which(!repeat_digit & monotonic)|>length()

two_digit <- vec|>map(~.x|>str_extract_all("(\\d)\\1+")|>unlist()|>nchar()|>min()==2)|>unlist()

#Part2
which(two_digit & monotonic)|>length()
