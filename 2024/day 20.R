source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(20,2024)
tst<-clipr::read_clip()

make_graph <- function(x){ 
  len <- nchar(x[[1]]) 
  x|> str_extract_all(".")|>
    unlist()|>
    matrix(ncol =len, byrow = TRUE) 
}
#Fast way to get complex vec of each occurrence of sym in grid.
to_complex <-function(sym,grid){
  which(grid == sym,a = T)|>apply(1,\(x)x[2]+x[1]*1i)
}

#Go backward, scoring the paths as we go along in terms of dist.
q <- collections::queue()
visited <- collections::dict()

#Get the normal times
visit_path <- function(x){
  q$clear()
  visited$clear()
  x<- make_graph(x)
  wall <- to_complex("#",x)
  
  q$push(to_complex("E",x)) #Init queue
  visited$set(to_complex("E",x),0)
  while(q$size()>0){
    pos <- q$pop()
    nxt <- pos + (0+1i)^(1:4)
    nxt <- nxt[-which(nxt %in% wall| map_lgl(nxt,~visited$has(.x)))] #Dont visit a wall or re-visit a cell
    if(length(nxt)==0) next 
    print(nxt)# show speed
    map(nxt,~visited$set(.x, visited$get(pos)+1)) #add each next to visited 
    map(nxt,~q$push(.x)) #And add to the queue.
  }
}

cheat<-function(x,p){
x<-make_graph(x)
df <- tibble(
  cells = visited$as_list()|>names()|>as.complex(),
  time = visited$as_list()|>unname()|>unlist()
)

compute_cheat <- cross_join(df,df)|>
  mutate(dist = abs(Re(cells.x - cells.y))+abs(Im(cells.x-cells.y)))|>
  filter(dist <= p)|>
  mutate(time_save = time.y-time.x-dist)|>
  filter(time_save>=100)
nrow(compute_cheat)
}



part_1 <-function(x){
visit_path(x)
cheat(x,2)
}


part_2 <-function(x){#Already visited, no need to repeat
cheat(x,20)
}

#Answers:
part_1(data) #1422

part_2(data) #1009299
