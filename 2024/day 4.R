source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
library(unpivotr)
data<- get_aoc(4,2024)
 
to_matrix <- function(data){ matrix(data|>strsplit("")|>unlist(),nrow = nchar(data[[1]]),ncol = length(data))} #get data as character matrix
collapse_matrix <- function(mtrx){ split(mtrx,row(mtrx))|>lapply(paste0,collapse="") } #concatenate matrix
collapse_diagonal <- function(data){ split(data,row(data)-col(data))|>lapply(paste0,collapse="")} #concatenate the diagonals
find_xmas <- function(str_list){ c(str_list|>str_extract_all("XMAS")|>unlist(),str_list|>str_extract_all("SAMX")|>unlist())|>na.omit() } #regex used to get the forwards / backwards xmas


part_1 <- function(data){ 
left_right <- data|>to_matrix()|>collapse_matrix()|>find_xmas() 
up_down <- data|>to_matrix()|>t()|>collapse_matrix()|>find_xmas() 
diag1 <- data|>to_matrix()|>collapse_diagonal()|>find_xmas() 
diag2 <- data|>to_matrix()|>t()|>apply(2,rev)|>collapse_diagonal()|>find_xmas() # need rev since transposing duplicates values.. c(left_right,up_down,diag1,diag2) } part_1(data)|>length()
c(left_right,up_down,diag1,diag2)|>length()
}


#I cant think of a more sensible way to do this, so lets play pictionary!

get_cross <- function(three_by_three){ three_by_three[col(three_by_three)%%2&row(three_by_three)%%2] } #returns the corners of a 3x3 matrix centered on an A

make_3x3 <- function(a_locations,mat){ #returns a list of 3x3 matrices given a list of x,y co-ords
  map(a_locations, \(x)mat[(x[1]):(x[1]+2),(x[2]):(x[2]+2)])
}

#cross 
check_cross <- function(cross){ #we don't have the center, so just check the corners match one of the possible permutations 
  all(cross ==c("S","M","S","M"))|
    all(cross ==c("M","S","M","S"))|
    all(cross ==c("M","M","S","S"))|
    all(cross ==c("S","S","M","M"))
}


part_2 <-function(x){
mat <- x|>to_matrix()|>t()
rows <- mat|>nrow()
cols <- mat|>ncol()
pad_mat <- matrix("O",nrow = rows +2, ncol= cols+2) #prevent out of bounds issue.
pad_mat[1:rows+1, 1:cols+1]<-mat
a_locations <- which(mat == "A", arr.ind = TRUE)
a_locations <- split(a_locations, row(a_locations))
patterns <- make_3x3(a_locations,pad_mat)
cross <- map(patterns,get_cross)
sum(unlist(map(cross, check_cross)))
}

#Answers:
part_1(data) # 2534
part_2(data) # 1866 

