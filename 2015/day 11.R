library(tidyverse)
data <- clipr::read_clip()

non_overlap_pair <- function(data){str_extract_all(data, "([a-z])\\1")|>lapply(unique)|>map(~length(.x)==2)}

iol <- function(data){is.na(str_extract(data, "[iol]"))}

abc <- function(data){!is.na(str_extract(data, "abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz"))}

next_char <- function(x){
 letters[which(letters == x ) + 1L]
}


tst|>lapply(next_char)
next_word <- function(x){
  dx<- x|>str_extract_all(".")|>unlist()
  newx <- dx|>lapply(next_char)|>unlist()
  count_z <- which(is.na(newx))
  if(!is.na(tail(newx,1))){paste0(c(dx|>head(-1),newx|>tail(1)),collapse = "")}else{
   no_increments <- (rle(is.na(rev(newx))))[[1]][[1]]
   if(no_increments == length(dx)){stop("hit all zzz's time to sleep..")}else{
     paste0(c(dx|>head(-no_increments-1), newx|>head(no_increments)|>tail(1),rep("a",no_increments)),collapse="")     
   }
   }
}

add_to_list <- function(lst,i){
  print(i)
  lst[[i+1]]<-next_word(lst[[i]])
  lst
}

build_n_words <- function(word,n){
  word_list <- c(word)
  
  reduce(1:n, add_to_list, .init= c(word,rep("",n)))
  
}

test_data <- build_n_words(data, 10^5)

#test_data |>write_rds(here::here("2015day11_100kwords.rds"))
test_str <- function(tst){
  if(all(c(iol(tst),abc(tst),non_overlap_pair(tst)))){return(tst)}
  
}

part_1 <-test_data|>lapply(test_str)|>unique()|>head(1)
