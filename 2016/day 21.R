source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(21,2016)

parse_data <- function(x){
  #Collapse the frst two words (these become function names) and collect the 
  #parameters (letters / numbers) which in this instance are 1 character long. 
  str_match_all(x,"\\w+")|> 
    map(\(y) c(y[1:2]|>paste0(collapse = "_"),
               y[map_lgl(y,~nchar(.x)==1)])
        ) 
}

swap_position <- function(x,y,string){
  x <- as.numeric(x)+1 
  y <- as.numeric(y)+1 
  tmp <- string[x] 
  string[x] <- string[y]
  string[y] <- tmp 
  string 
}

reverse_positions <- function(x,y,string){
  x <- as.numeric(x)+1 
  y <- as.numeric(y)+1 
  bfr <- vector("character") 
  if(x>0) bfr <- string[0:(x-1)] 
  inv <- rev(string[x:y]) 
  aft <- vector("character") 
  if(y <length(string)) aft <- string[(y+1):length(string)] 
  c(bfr,inv,aft) 
  } 

rotate_right <- function(x,string){ 
  x <- as.numeric(x) 
  if(x == 0) return(string) 
  c(tail(string,x),head(string,-x)) 
  } 

rotate_left <- function(x,string){ 
  x <- as.numeric(x) 
  if(x == 0) return(string) 
  c(tail(string,-x),head(string,x)) 
  } 

swap_letter <- function(x,y,string){
  x_loc <- which(string == x) 
  y_loc <- which(string == y) 
  string[x_loc] <- y 
  string[y_loc] <- x 
  string 
  } 

move_position <- function(x,y,string){
  x <- as.numeric(x)+1 
  y <- as.numeric(y)+1 
  tmp_x <- string[x] 
  tmp <- string[-x]
  if (y > length(tmp)) return(c(tmp, tmp_x)) 
  if (y == 1) return(c(tmp_x, tmp)) 
  bfr <- tmp[1:(y - 1)] 
  aft <- tmp[y:length(tmp)] 
  c(bfr, tmp_x, aft) 
  } 

rotate_based <- function(x, string) { 
  x_loc <- which(string == x) 
  steps <-  x_loc + (x_loc >= 5) #Simplified logic for R's 1 index
  rotate_right(steps , string) 
  } 

part_1 <- function(x){
  instrs <- parse_data(x) 
  out <- reduce(instrs, \(acc,nxt){
    cat(paste("calling",nxt[1],"with",nxt[-1], "and",glue::glue_collapse(acc),"\n"))
    do.call(nxt[1], args = list(nxt[-1])|> flatten()|> append(list(acc)))
    },.init=letters[1:8]
    ) 
  
  out|>paste0(collapse = "")|>noquote() 
  } 



part_2 <- function(x){ 
instrs <- parse_data(x) 
perms <- gtools::permutations(8,8,letters[1:8])

#Set up for parrallell runs ! 
library(furrr)
plan(multisession)

out <- future_map(1:nrow(perms), \(i) {
  reduce(instrs, \(acc, nxt) {
    do.call(nxt[1], args = list(nxt[-1]) |> flatten() |> append(list(acc)))
  }, .init = perms[i, ]) |> paste0(collapse = "")
},
.options = furrr_options(globals = list(
  reduce = reduce,
  flatten = flatten,
  instrs = instrs,
  swap_position = swap_position,
  reverse_positions = reverse_positions,
  rotate_right = rotate_right,
  rotate_left = rotate_left,
  swap_letter = swap_letter,
  move_position = move_position,
  rotate_based = rotate_based
)),
.progress = TRUE
)

perms[which(out == "fbgdceah"),] 
}


#Answers:
part_1(data) # bdfhgeca
part_2(data) # gdfcabeh #The third solution.... 
 