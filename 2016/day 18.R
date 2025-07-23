source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(18,2016)

to_lgl <- function(x){
  x|>str_extract_all(".")|>first()%>%{. == "."} 
  } 

left <- function(vec){ lag(vec)|>replace_na(T)}
right <- function(vec){ lead(vec)|>replace_na(T)} 

is_safe <- function(x) { 
  lft <- left(x) 
  rht <- right(x) 
  pmap_lgl(
    list(x, lft, rht),
    \(cen, lft, rht) {
      chk <- c(lft, cen, rht) 
      all(chk == c(T, T, T))|
        all(chk == c(F, T, F))|
        all(chk == c(F, F, F))|
        all(chk == c(T, F, T)) 
      }) 
  } 

count_safe <- function(x, distance) {
  vec <- to_lgl(x) 
  init <- list(vec,sum(vec))
  reduce(1:distance,
         \(acc, nxt) {
           print(c(nxt,acc[[2]])) #For progress viewing. 
           acc[[1]] <- is_safe(nth(acc,1)) 
           acc[[2]] <- acc[[2]] + sum(acc[[1]]) 
           acc 
           },
         .init = init)|>pluck(2) 
} 

part_1 <- function(x) count_safe(x,39) 
part_2 <- function(x) count_safe(x,399999) 

#Answers:
part_1(data) #1982 
#P2 is slow af - should implement a cache and determine the cycle..   
part_2(data) #20005203
