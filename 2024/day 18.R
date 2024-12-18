source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(18,2024)

parse_bytes <- function(x){
  x|>str_extract_all("\\d+")|>
    lapply(as.numeric)|>
    lapply(\(x) x[1] + x[2]*1i)
}

bfs <- function(current=0+0i, available,target,idx=1){
if(DONE){return(NULL)}#Early Break system (absolute hack)
  new_current <- map(available, ~.x*(abs(.x - current)==1))|>
    unlist()|>unique()
  if(target %in% new_current){return(idx)} #We're done
  if(length(available) == 0 & !any(target != current)){
    assign("DONE", TRUE, envir = globalenv())
    return(NULL)}
  
  available<- available[-which(available %in% c(new_current,current))]

  bfs(new_current,available,target,idx+1)
}

get_bytes <- function(bytes, n){
  bytes[1:n]|>unlist()
}

part_1 <-function(x, len,n){
  bytes <- parse_bytes(x)
  grid <- expand.grid(0:len,0:len)|>
  mutate(cplx= Var1 + 1i*Var2)|>
  pull(cplx)
  kbyte <- get_bytes(bytes,n)
  bfs(available = grid[-which(grid %in% kbyte)], target = len + 1i*len)
}

#I do too much here, but this is less time re-writing and more time heating my flat. 
part_2 <-function(x,len,start){
  bytes <- parse_bytes(x)
  grid <- expand.grid(0:len,0:len)|>
    mutate(cplx= Var1 + 1i*Var2)|>
    pull(cplx)
  
assign("DONE", FALSE, envir = globalenv())# H A C K - My own defined Break from a map 

  first_break <- imap_lgl(
  start:length(bytes), 
  \(x,i){
    print(i)#Progress bar
    is_null(bfs(
      available = grid[-which(grid %in% get_bytes(bytes,x))],
      target = len + 1i*len)
      )}
    )|>which()|>min()
paste0(Re(bytes[[start+first_break-1]]),",",Im(bytes[[start+first_break-1]]))|>noquote()
}

#Answers:
part_1(data,70,1024) # 324
part_2(data,70,1025) # 46,23
