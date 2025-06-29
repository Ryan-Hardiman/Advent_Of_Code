source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(8,2016)

#Lets just break it into words for now and remove words "x/y" and "by"
parse_data <- function(x){
  x%>%str_extract_all("\\w+")%>%lapply("[",-c(3,5))
}

rect <- function(mat,x){
digits <- x%>%str_extract_all("\\d+")%>%unlist()%>%as.numeric()
mat[1:digits[2],1:digits[1]]<- mat[1:digits[2],1:digits[1]] + 1
mat
}

rot_vec <-function(v,n){
    l <- length(v)
    tmp <- v[(l - n + 1):l]
    v[(n + 1):l] <- v[1:(l - n)]
    v[1:n] <- tmp
    v
}

rotate <- function(mat,...){
  dat<-list(...)
  r_c <- dat[[1]][1]
  x <- dat[[1]][2]
  amt <- dat[[1]][3]
  if(r_c=="row"){
    mat[as.numeric(x)+1,]<-rot_vec(mat[as.numeric(x)+1,],as.numeric(amt))
  }else{
    mat[,as.numeric(x)+1]<-rot_vec(mat[,as.numeric(x)+1],as.numeric(amt))
  }
  mat
}

part_1 <-function(x){
on <-x%>%parse_data()%>%reduce(
  \(acc,nxt){
    do.call(nxt[1],list(mat =acc,nxt[-1]%>%unlist()))
  },.init = matrix(0,6,50))>0
sum(on)
}

#TBH without hardcoding something - I'm unsure how you'd solve this fully in R.
#Paste into excel and whack a colour gradient on it and call it a day.. 
part_2 <-function(x){
x%>%parse_data()%>%reduce(
    \(acc,nxt){
      do.call(nxt[1],list(mat =acc,nxt[-1]%>%unlist()))
    },.init = matrix(0,6,50))%>%clipr::write_clip()
}

#Answers:
part_1(data) #106
part_2(data) #CFLELOYFCS
