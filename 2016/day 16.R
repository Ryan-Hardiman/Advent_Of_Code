source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(16,2016)

parse_data <- function(x){
  str_extract_all(x,".")%>%
    first()==1
}

curve <- function(x,len){
  new_x <- c(x,0,!rev(x))
  if(length(new_x)>=len) return(new_x)
  curve(new_x,len)
}

check <- function(x,len){
  x<-head(x,len) 
  if(length(x)%%2==1) return(noquote(paste0((!x)*1,collapse="")))
  odd <- x[seq(1,length(x),by=2)]
  even <- x[seq(2,length(x),by=2)]
  x<-map2_lgl(odd,even,xor)
  check(x,len)
}

part_1 <-function(x){
x%>%parse_data()%>%curve(272)%>%check(272)
}


part_2 <-function(x){
  x%>%parse_data()%>%curve(35651584)%>%check(35651584)
}

#Answers:
part_1(data) #10100011010101011
part_2(data) #01010001101011001
