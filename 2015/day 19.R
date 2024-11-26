source("get_aoc.R")
library(tidyverse)
data <-get_aoc(19,2015)
input <- data|>tail(1)
args <- data|>head(-2)

matches <- args|>str_extract("\\w+")
replacements <- args|>str_extract("\\w+$")
get_instances <- function(find,input){map(find, ~stringi::stri_locate_all(input,fixed =.x))|>unlist(recursive = FALSE)}

tmp <- function(a,b){
  if(is.na(a)){NA}else{
    paste0(substr(input,0,a-1),y,substr(input,b+1 ,nchar(input)))
  }
}


repl_str <- function(data,input, rev=FALSE){
  inst <- if(rev==FALSE){get_instances(matches,input)}else{get_instances(replacements,input)}
  over <- if(rev==FALSE){list(replacements,inst)}else{list(matches,inst)}
  pmap(
    over, 
       \(x,y){
         map2(y[,"start"],y[,"end"], \(a,b){#tmp(a,b) #use Dim names to my advantage.
           if(is.na(a)){NA}else{
             paste0(substr(input,0,a-1),x,substr(input,b+1 ,nchar(input)))
           }})})|>
    unlist()|>na.omit()|>unique()
}

#Part1
repl_str(args,input)|>length()

reduce_patterns <- function(drug=input,below){
  new_drug<-repl_str(args,drug,rev=T)
  new_drug[nchar(new_drug)<=below]
}

reduce_pattern_wrapper <- function(drug_list=input,i=0){
  if(any(drug_list=="e")){return(i)}
  print(i)
  i<-i+1
  below <- min(nchar(drug_list))
  drug_list <-drug_list|>map(~reduce_patterns(.x,below))|>unlist()|>unique()
  print(length(drug_list))
  reduce_pattern_wrapper(drug_list[order(nchar(drug_list))]|>head(100),i) #Cheeky trick - lets assume we will be taking one of the 100th smallest ouputs at each stage.
}



part2<-reduce_pattern_wrapper()
part2