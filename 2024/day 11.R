source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
library(memoise)
data <-get_aoc(11,2024)|>str_extract_all("\\d+")|>unlist()|>as.numeric()

to_memoise <- function(x) {
  if (x == 0) { 1 } else 
    if (nchar(x) %% 2 == 0) {
      as.numeric( c( 
        str_extract(x, paste0("^\\d{", nchar(x) / 2, "}")),
        str_extract(x, paste0("\\d{", nchar(x) / 2, "}$")) 
        ) ) 
      } else{
        x * 2024 
        } 
  } 

memod <- memoise(to_memoise)

#We had 'n' many "x's" which generated the 'rle1', so we need to multiply the 
#lengths by 'n' 
multiply_rle_lengths <- function(rle_, n){
  rle_[["lengths"]]<-rle_[["lengths"]]*n 
  rle_ 
  } 

#This is to avoid writing rep(x , y) since y will be very large, 
#and without it we would end up with arrays with +billions of repeated values. 
#Instead we summarize over the unique list of values (using tibbles because that 
# is how I could do it fastest). 
combine_rles<-function(rle_list){
  values <- lapply(rle_list, pluck,"values")|>unlist() 
  lengths <- lapply(rle_list, pluck,"lengths")|>unlist() 
  out<-tibble(values,lengths)|>group_by(values)|>
    reframe(values, sum(lengths))|>
    unique()|>
    rename(lengths =`sum(lengths)`)|>
    as.list() 
  
  attr(out, "class") <- c("rle") 
  
  out 
} 

#For each value in an input RLE (acc), find the new mapped values (1 or two elements) 
#Multiply by the previous count using multiply_rle_lengths 
#Combine list of rle's using combine_rles 

rules <- function(acc,nxt){ 
  map2(acc$lengths,
       acc$values,
       ~memod(.y)|>rle()|>
         multiply_rle_lengths(n=.x))|>
    combine_rles()
  } 
#wrapper- since part 2 is an exctension of part 1 
go <-function(x,length){ reduce(1:length, rules, .init = rle(x))|>pluck("lengths")|>sum() } 

#part_1 # 216996 
go(data,25) 
#part_2 # 257335372288947 
go(data,75)