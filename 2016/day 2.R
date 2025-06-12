source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(2,2016)

#Make directional formula' for button grid.
L <-function(x){
  if(x%%3==1){x}else{x-1}
}

R <- function(x){
  if(x%%3==0){x}else{x+1}
}

D <- function(x){
  if(x>6){x}else{x+3}
}

U <- function(x){
  if(x<4){x}else{x-3}
}

parse_data <- function(x){
  x%>% str_extract_all(".")
}

do_sequence <- function(x,init){
  reduce(x,\(acc,nxt)do.call(nxt,list(acc)),.init = init)
}

part_1 <-function(x){
x%>%parse_data()%>%
    accumulate(\(acc,nxt) do_sequence(nxt,acc),.init = 5)%>%
    tail(-1)%>%
    paste0(collapse = "")%>%
    as.numeric()
}

#Answers:
part_1(data) #44558


#Re-Make directional formula' for button grid.
U <- function(x){
  if(x %in%c("5","2","1","4","9")){x}else{
    case_when(
      x=="3"~"1",
      x=="6"~"2",
      x=="7"~"3",
      x=="8"~"4",
      x=="A"~"6",
      x=="B"~"7",
      x=="C"~"8",
      x=="D"~"B"
    )
  }
}

D <- function(x){
  if(x %in%c("5","A","D","C","9")){x}else{
    case_when(
      x=="1"~"3",
      x=="2"~"6",
      x=="3"~"7",
      x=="4"~"8",
      x=="6"~"A",
      x=="7"~"B",
      x=="8"~"C",
      x=="B"~"D"
    )
  }
}

L <- function(x){
  if(x %in%c("5","2","1","A","D")){x}else{
    case_when(
      x=="6"~"5",
      x=="3"~"2",
      x=="7"~"6",
      x=="B"~"A",
      x=="4"~"3",
      x=="8"~"7",
      x=="C"~"B",
      x=="9"~"8"
    )
  }
}

R <- function(x){
  if(x %in%c("1","4","9","C","D")){x}else{
    case_when(
      x=="5"~"6",
      x=="2"~"3",
      x=="6"~"7",
      x=="A"~"B",
      x=="3"~"4",
      x=="7"~"8",
      x=="B"~"C",
      x=="8"~"9"
    )
  }
}


part_2 <-function(x){
  x%>%parse_data()%>%
    accumulate(\(acc,nxt) do_sequence(nxt,acc),.init = "5")%>%
    tail(-1)%>%
    paste0(collapse = "")%>%
    noquote()
}

#Answers:
part_2(data) #6BBAD
