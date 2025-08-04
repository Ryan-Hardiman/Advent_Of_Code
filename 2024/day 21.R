source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(21,2024)
tst<-clipr::read_clip()
numpad <- set_names(c(1:3+1i, 1:3+2i,1:3+3i,2:3+4i),c(7:9, 4:6,1:3,0,"A"),)
dirpad <- set_names(c(2:3+1i,1:3+2i),c("^","A","<","v",">"))

move <- function(enter, pos="A"){
  out <- vector("list",nchar(enter))
  enter <- unlist(str_extract_all(enter,""))
  if(any(grepl("\\d",enter))){
    void <- 1+4i
    active_pad <- numpad
  }else{
    void <- 1+1i
    active_pad <- dirpad
  }
  
  add<- c("^"=-1i,"<"=-1,"v"=1i,">"=1)
  
  moves <- active_pad[enter]
  active <- active_pad[pos]
  
  for(i in seq_along(enter)){
    col_delta <- Re(active - moves[i])
    row_delta <- Im(active - moves[i])
    
    new_moves <- c(
      rep(ifelse(col_delta>0,"<",">"),abs(col_delta)),
      rep(ifelse(row_delta>0,"^","v"),abs(row_delta))
      )
    
    new_moves <- list(new_moves,rev(new_moves))|>unique()
  
    new_moves <- new_moves[
      sapply(
        new_moves,
        \(x) all(active + cumsum(add[x]) != void)
        )
    ]
    
    out[[i]] <- sapply(new_moves,glue::glue_collapse)
    active <- moves[i]
  }
  #Make permutations of instructions (using outer). 
  #For each instruction, separate with an "A" and always end with an "A"
  reduce(out,\(acc,nxt) as.character(outer(acc,nxt, \(x,y) paste(x,y,sep = "A"))))|>paste0("A")
  
}


part_1 <-function(x){
map_dbl(x,\(x){
numterminal  <- move(x)
dirterminal1 <- lapply(numterminal, move)|>unlist()
dirterminal2 <- lapply(dirterminal1,move)|>unlist()
min(nchar(dirterminal2))*as.numeric(str_extract(x,"\\d+"))
})|>sum()
}


#We imagine at each stage this next move is our last, and keep only those with
#the shortest length. This way the number of variants doesn't explode.
#As we expand, we will eventually capture the value we want, and we stop there.
find_optimal_move <- function(goal, init){
  out <- move(x,y)
  if (length(out) == 1) return(out) #Only one way to do it
  
  if (goal == "A" & init == "v") return(c("^>A", ">^A")[choice_1]) 
  if (goal == "v" & init == "A") return(c("v<A", "<vA")[choice_2])
  if (goal == "^" & init == ">") return(c("<^A", "^<A")[choice_3])
  if (goal == ">" & init == "^") return(c("v>A", ">vA")[choice_4])
  
  move_length <- map_dbl(str_replace(out,"A", ""), \(a) move(a)[1]|>nchar())
  out[which.min(move_length)]
}

f<- function(choice_1,choice_2,choice_3,choice_4){

 dirpad_moves <- map(names(dirpad),
                     \(x) sapply(names(dirpad),
                                 \(y) find_optimal_move(y, x)
                                 )
                     )|>unlist()
 #5 buttons, so try each new position from every one of the 5.
 names(dirpad_moves) <- paste0(rep(names(dirpad),each=5),names(dirpad_moves))
 
 map_mat <- t(sapply(names(dirpad_moves), \(x) as.integer(grepl(x, paste0("A", dirpad_moves), fixed = TRUE))))
 colnames(map_mat) <- rownames(map_mat)
 
 code_seq3 <- function(enter, depth) {
   code1 <- paste0("A", move(enter))
   code1 <- strsplit(code1, "")
   
   
   out <- sapply(code1, function(etr) {
     y <- paste0(etr[-length(etr)], etr[-1])
     x <- integer(25L)
     names(x) <- colnames(map_mat)
     x[names(table(y))] <- table(y)
     x <- x[rownames(map_mat)]
     sum(Reduce(`%*%`, rep(list(map_mat), depth)) %*% x)
   })
   
   min(out) * as.numeric(str_extract(enter,"\\d+"))
   
 }
 sum(map_dbl(x, move, n = 25))
 
 }
#Okay, so it's clearly too big to just naively do all permutations. 
#So need to pre-optimize the solution?
part_2 <-function(x){
  #Generate permutation of choices
  choices <- expand.grid(choice_1=1:2,choice_2=1:2,choice_3=1:2,choice_4=1:2)
  
  
}

#Answers:
part_1(data) #
part_2(data) #
