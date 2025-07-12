source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(13,2016)

make_vec <- function(x){ 
  outer(0:x,0:x,\(x,y) complex(1,x,y))|>
    c()|>
    set_names() 
} 

adjust_values <- function(vec, input){ 
  vec|>map_dbl(\(x) Re(x)^2 + 3*Re(x) + 2*Re(x)*Im(x) + Im(x) + Im(x)^2 + input) 
} 


collect_gaps <- function(x){ 
  even <- map_lgl(x,\(x) x|>intToBits()|>as.integer()|>sum()%%2==0) 
  even[which(even)]|>names()|>as.complex() 
} 

get_neighbours <- function(x,vec){ 
  matches <- map_lgl(vec,\(y) abs(Re(x)-Re(y))+ abs(Im(x)-Im(y)) == 1)
  vec[which(matches)] 
} 

bfs<- function(current,available,goal,dist,end_cond=-1){ 
  new_current <- map(current,\(x) get_neighbours(x,available))|>
    unlist()|>
    unique() 
  
  current <- c(current,new_current)
  if(goal %in% new_current) return(dist+1) 
  
  available <- setdiff(available,new_current) 
  if(length(available) == 0) return(dist+1) 
  if(dist+1 == end_cond) return(current)
  bfs(current, available,goal,dist+1,end_cond) 
} 


part_1 <-function(x){
  gap_vec <- make_vec(100)|>adjust_values(parse_integer(x))|>collect_gaps() 
  bfs(as.complex(1+1i),gap_vec, as.complex(31+39i),0) 
}


part_2 <-function(x){
  gap_vec <- make_vec(100)|>adjust_values(parse_integer(x))|>collect_gaps() 
  bfs(as.complex(1+1i),gap_vec, as.complex(-1-1i),0,50)%>%unique()%>%length()
}

#Answers:
part_1(data) #82
part_2(data) #138
