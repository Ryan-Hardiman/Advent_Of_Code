source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
library(unpivotr)
#library(prodlim)
data<-  get_aoc(6,2024)
tst<- clipr::read_clip()
              
check_log <- function(new,log){
  map(log,~all(.x == new))|>unlist()|>any()
}



move <- function(data,
                 objects,
                 row_start,
                 col_start,
                 dir,
                 log ,
                 do_part_2 = FALSE){
if(dir == -1+0i) new_obj <-objects|>filter(row==row_start, col<col_start) #left
if(dir == 1+0i) new_obj <-objects |>filter(row==row_start,col>col_start) #right
if(dir == -1i) new_obj <-objects|>filter(row>row_start,col==col_start) #down
if(dir == 1i) new_obj <-objects|>filter(row<row_start,col==col_start) #up

if(nrow(new_obj)==0 & do_part_2 == FALSE){# nothing hit - need to add on the extra distance
  if(dir == 1){return(c(log,list(c(row = row_start, col =max(data$col)))))}
  if(dir == -1){return(c(log,list(c(row = row_start, col =min(data$col)))))}
  if(dir == 1i){return(c(log,list(c(row = min(data$row), col =col_start))))}
  if(dir == -1i){return(c(log,list(c(row = max(data$row), col =col_start))))}
} else if(nrow(new_obj)==0 & do_part_2 == TRUE){return(NULL)}

if(dir == -1) new_obj <- new_obj |>filter(col == max(col))|>mutate(col = col+1, dir = dir)
if(dir == 1) new_obj <- new_obj |>filter(col == min(col))|>mutate(col = col-1, dir = dir)
if(dir == -1i) new_obj <- new_obj |>filter(row == min(row))|>mutate(row = row-1, dir = dir)
if(dir == 1i) new_obj <- new_obj |>filter(row == max(row))|>mutate(row = row+1, dir = dir)

if(check_log(new_obj,log)==TRUE){return(c(log,new_obj|>mutate(dir = dir)|>unlist()|>list()))}#Loop occurred
new_log <- c(log,new_obj|>unlist()|>list())
move(data,objects,new_obj$row,new_obj$col,dir*-1i,new_log,do_part_2)
}



map_log <- function(log){
  if(is.null(log)){return(NULL)}
  map2(head(log,-1),tail(log,-1),\(x,y) 
       tibble(row = pluck(x,"row"):pluck(y,"row"), col = pluck(x,"col"):pluck(y,"col")))|>
    reduce(rbind)|>arrange("row","col")
}

make_grid <- function(x){
 x|>str_extract_all("")|>tibble()|>unnest_wider(`str_extract_all(x, "")`,names_sep = "")|>as_cells()
}

traverse_area <- function(x,obj, new_object =NULL,do_part_2=FALSE){
  if(!is.null(new_object)){obj<- rbind(obj, tibble(row = new_object[[1]], col = new_object[[2]], data_type = "chr", chr = "#"))}
  start <- x|>filter(chr == "^")
  log<- move(x,obj,start$row, start$col,dir =1i, list(unlist(start|>mutate(dir= 1i))), do_part_2)
  if(do_part_2==FALSE){log|>map_log()|>unique()}else{log|>map_log()}
}

part_1 <-function(x){
grid <- make_grid(x)
obj <-  grid[which(grid$chr=="#"),]
traverse_area(grid,obj)|>nrow()
}


#We only need to place obstructions along the current path (less the current position)
part_2 <-function(x){
grid <- make_grid(x)
obj <-  grid[which(grid$chr=="#"),]
current_path <- traverse_area(grid,obj)|>tail(-1)
current_path<- current_path|>split(row_number(current_path))
paths <- imap(current_path, \(y, idx) {
  print(idx) #Progression viewer
  traverse_area(grid, obj, y, TRUE)
})
paths[ which(paths|>lapply(is.null)|>lapply(`!`)|>unlist())]|>length()
}

#Answers:
part_1(data) # 5145
part_2(data) # 1523





