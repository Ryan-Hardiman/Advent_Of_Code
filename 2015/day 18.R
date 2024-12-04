source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data <- get_aoc(18,2015)|> str_replace_all("#","1")|> str_replace_all("\\.","0")|> str_extract_all(".")|>unlist()|>matrix(ncol = 100) 
data <- tibble(value = data|>c()|>as.numeric(), row = which(!is.na(data), arr.ind = TRUE)[,1], col =which(!is.na(data), arr.ind = TRUE)[,2] ) 
data 

perimeter <- tibble(value = 0, row = c(0:101,0:101,rep.int(0,102),rep.int(101,102)), col =c(rep.int(0,102),rep.int(101,102),0:101,0:101))|>unique() 

board <- rbind(data, perimeter)|>mutate(across(everything(), as.numeric)) 
make_boards <- function(board){ 
  down_board <- board |>mutate(row = row +1) 
up_board <- board |>mutate(row = row -1) 
right_board<- board |>mutate(col = col +1) 
left_board <- board |>mutate(col = col -1) 
lu_board <- board |> mutate(row = row -1, col = col-1) 
ld_board <- board |> mutate(row = row +1, col = col-1) 
ru_board <- board |> mutate(row = row -1, col = col+1) 
rd_board <- board |> mutate(row = row +1, col = col+1) 
boards <- list(board, up_board, down_board, left_board, right_board, lu_board, ld_board,ru_board, rd_board) 
boards
} 

boards <- make_boards(board) 

get_count <- function(x){
  neighbours <- x |> map(~ .x |> filter(row %in%c(1:100), col %in% c(1:100)))|>
    reduce(\(x,y) left_join(x,y, by = c("row", "col")))|>arrange(col,row)|>
    select(-c(row,col))|>rowSums() 
  
  tst <- x[[1]]|>filter(col %in% c(1:100), row %in% c(1:100))|>arrange(col,row)|>mutate(count =neighbours-value)
  tst 
  } 
#get_count(boards) 

next_light <- function(x, nxt){ 
  print(nxt) 
  cnt <- get_count(x)|>mutate(value = ifelse(value == 1 & count %in%c(2,3),1,ifelse(value == 0 & count == 3,1,0)))|>select(-count)
  board <- cnt |> rbind(perimeter) 
  boards <- make_boards(board)
  boards 
  } 
#Part 1 
part_1 <-reduce(1:100, next_light, .init = boards) 
part_1[[1]]|>pull(value)|>sum() 


#part 2 
board <- board |> mutate(value = ifelse((row %in% c(1,max(row))) & (col %in%c(1,max(col))) ,1,value )) 
boards <- make_boards(board) 
conway <- function(x, nxt){ 
  print(nxt) 
  cnt <- get_count(x)|>mutate(value = ifelse(value == 1 & count %in%c(2,3),1,ifelse(value == 0 & count == 3,1,0)))|>select(-count) 
  cnt <- cnt |> mutate(value = ifelse((row %in% c(1,max(row))) & (col %in%c(1,max(col))),1,value )) 
  board <- cnt |> rbind(perimeter) 
  boards <- make_boards(board) 
  boards } 

part_2 <-reduce(1:100, conway, .init = boards) 
part_2[[1]]|>pull(value)|>sum()