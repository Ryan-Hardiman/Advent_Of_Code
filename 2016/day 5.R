source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
library(cli) #At some point I promise to make a dummy example of this hash fn. 
data<- get_aoc(5,2016)


#This already reeks of "Do it for a trivial number", "Cool, now do it for 20Billion"  
#So lets prepare for it by "chopping" it into managable chunks.
recursive <- function(input,x, lst,goal_length){
  check <- paste0(input,x:(x+10^5))%>%hash_md5()
  to_add <- check[which(grepl("^0{5}",check))]%>%substr(6,6)
  lst <- c(lst,to_add)%>%head(8)
  #Break condition
  if(length(lst)==goal_length){return(lst)}
  rm(check)
  recursive(input,x+10^5+1,lst,goal_length)
}

#Well that was surprising. We're still looking for 8 vars (0-7) but now want 
# the FRIST of each of the numbers - taking the 7th value as its element. 
recursive2 <- function(input,x, lst){
  check <- paste0(input,x:(x+10^5))%>%hash_md5()
  check <- check[which(grepl("^0{5}",check))]
  no_to_add <- check%>%substr(6,6)
  el_to_add <- check%>%substr(7,7)
  #Recursively check if we can add it to the list, and if so do so.
  lst <- reduce2(no_to_add,el_to_add,\(acc,nxt_no,nxt_el){ 
    if(length(nxt_no)==0){return(acc)}
    if(!grepl("[0-7]",nxt_no)){return(acc)}
    if(is.na(acc[which(names(acc)==nxt_no)])){
      acc[which(names(acc)==nxt_no)]<-nxt_el}
      acc 
  },.init = lst )
  
  #Break condition - All non-NA
  if(!any(is.na(lst))){return(lst)}
  rm(check)
  recursive2(input,x+10^5+1,lst)
}


part_1 <-function(x){
recursive(x,0,NULL,8)%>%glue::glue_collapse()
}


part_2 <-function(x){
  recursive2(x,0,set_names(0:7)==NA)%>%glue::glue_collapse()
}

#Answers:
part_1(data) #4543c154
part_2(data) #1050cbbd



