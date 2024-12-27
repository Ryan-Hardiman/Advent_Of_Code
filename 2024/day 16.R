source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
library(collections)
data<- get_aoc(16,2024)
tst <- clipr::read_clip()

make_graph <- function(x){ 
  len <- nchar(x[[1]]) 
  x|> str_extract_all(".")|>
    unlist()|>
    matrix(ncol =len, byrow = TRUE) 
}

visited <- dict()
pq <- priority_queue()
#Let PQ be list of X-Y co-ord in complex form, Dir in complex form, score, and prev values.


a_star <- function(g_start,g_end,mat,part_2=FALSE){
  pq$clear()
  visited$clear()
  pq$push(list(xys=g_start,dir=1+0i,score=0,route=g_start),-abs(g_start-g_end))
  
  max_len <- Inf
  route <- c()
  while(pq$size()>0){
    curr <- pq$pop()
    if(curr[[3]]>max_len){break} #Shouldn't get hit but used as backup
    
    #Finished
    if(curr[[1]]==g_end){
      if(!part_2)return(curr[3]) #Part 1
      #If a shorter route is found afterwards, we need to start again.
      if(curr[[3]]==max_len){
        route<-c(route,curr[[4]])
      }else{
        max_len<-curr[[3]]
        route <-curr[[4]]
      }
      next
    }
    
    if(visited$has(str_flatten(curr[1:2],"~"))){
      if(!part_2 |curr[[3]]>visited$get(str_flatten(curr[1:2],"~"))) next}
    visited$set(str_flatten(curr[1:2],"~"),curr[[3]])
    
    #Forward
    if((curr[[1]]+curr[[2]]) %in% mat){
      pq$push(list(xys  =curr[[1]]+curr[[2]],
                   dir  =curr[[2]],
                   score=curr[[3]]+1,
                   route=c(curr[[4]],curr[[1]]+curr[[2]])),
              -1-curr[[3]]-abs(curr[[1]]+curr[[2]]-g_end)) #Queue score (higher = earlier execution) 
      }
    
    #Left
    if((curr[[1]]+curr[[2]]*(0+1i)) %in% mat ){
      pq$push(list(xys=curr[[1]]+curr[[2]]*(0+1i),
                   dir=curr[[2]]*(0+1i),
                   score=curr[[3]]+1001,
                   route=c(curr[[4]],curr[[1]]+curr[[2]]*(0+1i))),
              -1001-curr[[3]]-abs(curr[[1]]+curr[[2]]*(0+1i)-g_end)) #Queue score (higher = earlier execution) 
      }
    
    #Right
    if((curr[[1]]+curr[[2]]*(0-1i)) %in% mat){
      pq$push(list(xys=curr[[1]]+curr[[2]]*(0-1i),
                   dir=curr[[2]]*(0-1i),
                   score=curr[[3]]+1001,
                   route=c(curr[[4]],curr[[1]]+curr[[2]]*(0-1i))),
              -1001-curr[[3]]-abs(curr[[1]]+curr[[2]]*(0-1i)-g_end)) #Queue score (higher = earlier execution) 
      }
  }
length(unique(route))
}






part_1 <-function(x){
  grid <- x|>make_graph()
  g_start <- which(grid == "S", arr.ind = TRUE)
  g_start <- g_start[[2]]+1i*g_start[[1]]
  g_end <- which(grid == "E", arr.ind = TRUE)
  g_end <- g_end[[2]]+1i*g_end[[1]]
  mat <- which(grid == "."|grid == "E",arr.ind = TRUE)
  mat <- mat[,2] + mat[,1]*1i
  a_star(g_start,g_end,mat)
}


part_2 <-function(x){
  grid <- x|>make_graph()
  g_start <- which(grid == "S", arr.ind = TRUE)
  g_start <- g_start[[2]]+1i*g_start[[1]]
  g_end <- which(grid == "E", arr.ind = TRUE)
  g_end <- g_end[[2]]+1i*g_end[[1]]
  mat <- which(grid == "."|grid == "E",arr.ind = TRUE)
  mat <- mat[,2] + mat[,1]*1i
  a_star(g_start,g_end,mat,TRUE)
}

#Answers:
part_1(data) # 98520
part_2(data) # 609 
