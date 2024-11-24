source("get_aoc.R")
data<-get_aoc(1,2019)|>as.numeric()
#Part 1
(floor(data/3)-2)|>sum()

recursive_fuel <- function(x){
  n <- tail(x,1)
  if(floor(n/3)-2<=0)return(tail(x,-1))else return(recursive_fuel(c(x,floor(tail(x,1)/3)-2)))
}
#Part 2
data |>map(~.x|>recursive_fuel())|>unlist()|>sum()
