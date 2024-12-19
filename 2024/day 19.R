source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
library(collections)
data<- get_aoc(19,2024)


parse_data <- function(x){
  stripes <- x[[1]]|>str_extract_all("\\w+")|>unlist()
  designs <- x[-c(1:2)]
  list(stripes, designs)
}

#Memoisation was seemingly slow, so tried Cache instead and it was faster.. 
#Need to look into why - thought is computation is fast and so is dict lookup?
cache <- dict()
to_cache <- function(x,stripes){
  if(cache$has(x)){return(cache$get(x))}
  if(x == ""){return(1)}
  
  out<-0
  for (stripe in stripes){
    if(grepl(stripe,x)) out <- out + to_cache(str_remove(x,stripe),stripes)
  }
  
  cache$set(x,out)
  out
}



part_1 <-function(x){
x <- parse_data(x)
stripes <- paste0("^",x[[1]])
sum(sapply(x[[2]], to_cache,stripes)>0)
}


part_2 <-function(x){
  x <- parse_data(x)
  stripes <- paste0(x[[1]])
  sum(sapply(x[[2]], to_cache,stripes))
}

#Answers:
part_1(data) # 255
part_2(data) # 621820080273474
