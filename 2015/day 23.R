library(tidyverse)
data <- clipr::read_clip()

hlf <- function(x,idx){list(x/2,idx+1)}
tpl <- function(x,idx){list(x*3,idx+1)}
inc <- function(x,idx){list(x+1,idx+1)}
#jmp <- function(i,idx){list(idx+as.numeric(i))} not actually required
jio <- function(x,i,idx){if(x!=1){list(x,idx+1)}else{list(x,idx+as.numeric(i))}} #Fuck this function in particular
jie <- function(x,i,idx){if(x%%2==0){list(x,idx+as.numeric(i))}else{list(x,idx+1)}}


funcs <- data |>str_extract("\\w+")
args <- data |>str_extract_all("a|b|[-]*\\d+")

next_instruction <- function(a,b,idx){
  print(tibble(a=a,b=b,index=idx))
 if(is.na(funcs[idx])){return(b)}
  
if(args[[idx]][[1]]%in%c("a","b")){
  tmp <- eval(parse(text = 
                      glue::glue(
                        funcs[[idx]],"(", glue::glue_collapse(args[[idx]],","),",",idx,")"
                        )
  ))
  if(args[[idx]][[1]]=="a"){a<-tmp[[1]]}else{b<-tmp[[1]]}
  idx <- tmp[[2]]
  return(next_instruction(a,b,idx))  
  }else{return(next_instruction(a,b,idx+as.numeric(args[[idx]][[1]])))}
}

part_1 <- next_instruction(0,0,1)

part_2 <- next_instruction(1,0,1)
