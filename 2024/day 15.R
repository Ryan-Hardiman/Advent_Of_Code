source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(15,2024)


parse_data <- function(x,part_2=FALSE){
  x <- x|>split(cumsum(x==""))
  
  if (part_2 == FALSE) {
    grid <- x[[1]] |> make_graph()
  } else{
    grid <- x[[1]] |> make_graph2()
  }
  
  dirs <- x[[2]]|>unlist()|>
    paste0(collapse = "")|>str_extract_all(".")|>
    make_dirs()
  list(grid,dirs)
}

make_graph <- function(x){ 
  len <- nchar(x[[1]]) 
  x|> str_extract_all(".")|>
    unlist()|>
    matrix(ncol =len, byrow = TRUE) 
}

make_graph2 <- function(x){
  len <- nchar(x[[1]]) 
  x|>
    lapply(str_replace_all,"#","##")|>
    lapply(str_replace_all,"O","[]")|>
    lapply(str_replace_all,"\\.","..")|> #Ha good one Regex!
    lapply(str_replace_all,"@","@.")|>
    str_extract_all(".")|>
    unlist()|>
    matrix(ncol =2*len, byrow = TRUE) 
}

make_dirs <- function(x){
  x|>lapply(\(y) y |>
           str_replace_all("<","-1+0i")|>
           str_replace_all(">", "1+0i")|>
           str_replace_all("\\^","0-1i")|> #Ha good one Regex!
           str_replace_all("v",  "0+1i"))|>
    lapply(as.complex)|>
    unlist()
}






make_box_tree <- function(grid, pos_list,dir_sgn, level=1){
  new <- map(pos_list[[level]],
             \(x) 
             if(grid[x[1]+1*dir_sgn,x[2]]=="#"){list(NA)} else #Base case  
               if(grid[x[1]+1*dir_sgn,x[2]]=="[" ){list(c(x[1]+1*dir_sgn,x[2]),c(x[1]+1*dir_sgn,x[2]+1))}else #expand_left  
                 if(grid[x[1]+1*dir_sgn,x[2]]=="]" ){list(c(x[1]+1*dir_sgn,x[2]),c(x[1]+1*dir_sgn,x[2]-1))}else #expand_right
                   if(grid[x[1]+1*dir_sgn,x[2]]=="." ){list(Inf)} # Using third NA, NULL, INF etc type for dots 
  )|>list_flatten()
  
  if(lapply(new,\(x) any(is.na(x)))|>unlist()|>any()){return(NULL)} #If we hit any wall, stop.
  new <- new[lapply(new, \(x) !any(x == Inf))|>unlist()]|>unique() # Don't add dots (inf points.)
  if(length(new)==0){return(pos_list)} #if all the spots below are dots, we can move.
  make_box_tree(grid, c(pos_list, list(new)), dir_sgn, level+1)
}

update_grid_from_box_tree<- function(grid, box_tree,dir_sgn,init = FALSE){
  rows <- tail(box_tree,1)[[1]]|>lapply("[",1)|>unique()|>unlist()
  cols <- tail(box_tree,1)[[1]]|>lapply("[",2)|>unlist()
  
  if(init){#We know we can move everything down, so lets do so. 
    grid[rows[1]+dir_sgn, cols] <-grid[rows[1], cols] 
  }
  
  if(length(box_tree) == 1){
    grid[rows+dir_sgn,cols]<-"@"
    grid[rows,cols]<-"."
    return(grid)}

  search_cols <- head(tail(box_tree,2),1)[[1]]|>lapply("[",2)|>unlist()
  
  grid[rows+dir_sgn, cols]<-grid[rows, cols]
  grid[rows, cols]<-"."
  
  update_grid_from_box_tree(grid, box_tree|>head(-1),dir_sgn)
}



move <- function(grid, dir,part_2 =FALSE){
  rob_pos <-which(grid =="@", arr.ind = TRUE)
  rows <- max(row(grid))
  cols <- max(col(grid))
  
  #Logic here is to get a forwards list (irrespective of dir) of e.g "@..o...o.#"
  #Where @ is ALWAYS the first item. 
  look_forward <- grid[rob_pos[1]:min(max(Im(dir)*rows+rob_pos[1],1),rows), 
                       rob_pos[2]:min(max(Re(dir)*cols+rob_pos[2],1),cols)]
  
  if(!("."%in%look_forward)){return(grid)} # No dots, so no moves
  first_dot <- min(which(look_forward == "."))
  first_hash <- min(which(look_forward == "#"))
  if(first_dot>first_hash){return(grid)} #dots behind a wall, so no moves
  if(first_dot == 2 | Im(dir)==0 | part_2==FALSE){ #Simple case, move the objects one over, pad with a dot
    grid[rob_pos[1]:min(max(Im(dir)*rows+rob_pos[1],1),rows), 
         rob_pos[2]:min(max(Re(dir)*cols+rob_pos[2],1),cols)] <- c(".",look_forward[-first_dot]) #dot moves to front. 
    
  }else{
    box_tree <- make_box_tree(grid,list(list(rob_pos)),Im(dir)) # This is the list of the "@,[,]" which interact with the current rob_pos.
    if(is.null(box_tree)){return(grid)} #Some blockage - so return the grid again.
    grid <-update_grid_from_box_tree(grid, box_tree, Im(dir),TRUE)
  }
  grid
}



part_1 <-function(x){
  x <- x|>parse_data()
  to_score <- reduce(x[[2]], move, .init = x[[1]])
  ((row(to_score)-1)*100 + (col(to_score)-1))[which(to_score == "O")]|>sum()
}

part_2 <-function(x){
  x<-x|>parse_data(TRUE)
  to_score <- reduce(x[[2]], move, .init = x[[1]],part_2 = TRUE)
  print(to_score)
  ((row(to_score)-1)*100 + (col(to_score)-1))[which(to_score == "[")]|>sum()
}

#Answers:
part_1(data) #1371036


part_2(data) #1392847


