source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(13,2024)
tst <- clipr::read_clip()
parse_data <- function(x){
x<-x|>str_extract_all("\\d+")|>unlist()|>as.numeric()|>matrix()
out<- list(
x1 = x[row(x)%%6==1]|>unlist(),
x2 = x[row(x)%%6==3]|>unlist(),
y1 = x[row(x)%%6==2]|>unlist(),
y2 = x[row(x)%%6==4]|>unlist(),
z1 = x[row(x)%%6==5]|>unlist(),
z2 = x[row(x)%%6==0]|>unlist()
)
out
}

do_solve <- function(x1,x2,y1,y2,z1,z2){
pmap(list(x1,x2,y1,y2,z1,z2), 
     \(A,B,C,D,E,H) 
     solve(matrix(c(A,B,C,D),nrow=2,byrow = TRUE),
           matrix(c(E,H),nrow = 2))
     )
}



part_1 <-function(x){
  x<- parse_data(x)
  poss_solve <- pmap(list(x$x1,x$x2,x$y1,x$y2,x$z1,x$z2),do_solve)|>unlist(recursive = FALSE)
  poss_solve
  map(poss_solve, ~ .x*c(3,1)*((pmin(abs(c(.x %% 1, .x %% 1 - 1))) < 10^-3)|>sum()==2))|>unlist()|>sum()
}


part_2 <-function(x){
  x<- parse_data(x)
  poss_solve <- pmap(list(x$x1,x$x2,x$y1,x$y2,x$z1+1e+13,x$z2+1e+13),do_solve)|>unlist(recursive = FALSE)
  poss_solve
  map(poss_solve, ~ .x*c(3,1)*((pmin(abs(c(.x %% 1, .x %% 1 - 1))) < 10^-3)|>sum()==2))|>unlist()|>sum()
}

#Answers:
part_1(data) #33427
part_2(data) #91649162972270
