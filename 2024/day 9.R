source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(9,2024)

#Explanation for part 1:
#We extract the numbers and for every even indexed number we insert that
#many dots instead of the repeated value. We then use reduce on "switch_nums"
#which replaces the first nth dot with the backwards propagated nth number.
#We drop the "." acc values, but this is unnecessary in the long run. 
#We then take the imap over the list of numbers to get the result, remembering
#that R has index starting at 1 not 0.

num_dot <- function(x){
  if(x%%2==0){"."}else{ceiling(x/2)-1}
}

unwind_rle <- function(x){
  x|>str_extract_all("\\d")|>unlist()|>as.numeric()|>imap(\(x,idx)rep(num_dot(idx),x))|>unlist()
}

switch_nums<-function(acc,nxt){
  move_index <- which(acc!=".")|>tail(1)
  to_move <- acc[move_index]
  acc[nxt]<-to_move
  acc[-move_index]
}

clear_space <- function(x){
  nums <- x|>str_extract_all("\\d+")|>unlist()
  dots <- which(x==".")
  reduce(dots, switch_nums, .init = x)|>
    paste(collapse = ".")|>str_extract_all("\\d+")|>
    unlist()|>as.numeric()
}

part_1 <-function(x){
x|>unwind_rle()|>clear_space()|>
    imap_dbl(\(x,idx) as.numeric(x)*(idx-1))|>
    sum()|>format(scientific=FALSE)
}

#Explanation for part 2:
#We get the RLE of non "."'s,
#Then reverse the list to move the end numbers first. 
#Fragment space checks whether the un-wound "nxt" rle can fit in any gap
#BEFORE the first index of the "nxt" rle occurs on the disc. 

fragment_space <- function(acc,nxt_l, nxt_v){
x_rle <- rle(acc)
possible_index <- which(x_rle$lengths >=nxt_l & x_rle$values ==".")
if(possible_index|>length()>0){
  vec_index <- x_rle$lengths[1:(possible_index|>head(1)-1)]|>sum()+1
  if(vec_index<head(which(acc == nxt_v),1)){
    acc[which(acc == nxt_v)]<-"#"
    acc[vec_index:(vec_index + nxt_l-1)]<-nxt_v
  }
}
acc
}



part_2 <-function(x){
num_rle <- rev(unwind_rle(x))[which(rev(unwind_rle(x))!=".")]|>rle()
reduce2(num_rle$lengths, num_rle$values, fragment_space, .init = unwind_rle(x))|>
  str_extract_all("\\d+")|>imap(\(x,idx) (idx-1)*as.numeric(x))|>unlist()|>
  sum()|>format(scientific=FALSE)
}

#Answers:
part_1(data) # 6283170117911
part_2(data) #6307653242596

