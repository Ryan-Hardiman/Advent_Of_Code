source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(5,2024)


narrow_rules <- function(update,rules){
  rules|>filter(before%in%update & after %in% update)
}

check <- function(rules,update,unordered=0,part_2=FALSE){
  if(length(update)==1) {
    if (part_2 == TRUE) {
      if (unordered == 0) {return(0)} else{return(as.numeric(update))}
    } else{return(as.numeric(update))}
  }
  if(length(update)%%2==0){stop("NO MIDDLE")}
 start <- which(update%in% rules$before & !update%in% rules$after)
 end <- which(!update%in% rules$before & update%in% rules$after)
 if((start ==1 & end == length(update))){
   rules <- rules|>filter(before!=update[start],after!=update[end])
   update <-  update|>tail(-1)|>head(-1)
   check(rules,update,unordered,part_2)
   
 }else{
   if(part_2==FALSE){return(0)}
   rules <- rules|>filter(before!=update[start],after!=update[end])
   update <-  update[-c(start,end)]
   unordered<-unordered+1
   check(rules,update,unordered,part_2)
    }
}


part_1 <-function(data){
  rules <- separate_wider_delim(tibble(data =data[!is.na(data|>str_extract("\\|"))]),"data","|",names = c("before","after"))
  updates <- data[!is.na(str_extract(data,","))]|>str_extract_all("\\d+")
  sub_rules<- map(updates,~narrow_rules(.x,rules)) 
  map2_dbl(sub_rules,updates,check)|>sum()
}


part_2 <-function(data){
  rules <- separate_wider_delim(tibble(data =data[!is.na(data|>str_extract("\\|"))]),"data","|",names = c("before","after"))
  updates <- data[!is.na(str_extract(data,","))]|>str_extract_all("\\d+")
  sub_rules<- map(updates,~narrow_rules(.x,rules))
  map2_dbl(sub_rules,updates,\(x,y)check(x,y,part_2=TRUE))|>sum()
}

#Answers:
part_1(data) # 4609
part_2(data) # 5723

