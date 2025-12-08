source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(06,2025)

parse_data <- function(x){
  str_extract_all(x,"\\d+|[*+]")
}



#Takes a list e.g. 
#[[1]] 123 328 51 64
#[[2]] 45  64  387 23
#[[3]] 6   98  215 314
#[[4]] *   +   *   +
#
# and transforms it into a list like
#[[1]] 123 45  6   "*"
#[[2]] 328 64  98  "+"
#[[3]] 51  387 215 "*"
#[[4]] 64  23  314 "+"

rotate_list <- function(x, fill = "") {
  # lengths of each element
  max_len <- max(lengths(x))
  
  # pad each vector to same length
  padded <- lapply(x, function(v) {
    length(v) <- max_len
    v[is.na(v)] <- fill
    v
  })
  
  # rotate (transpose)
  out <- lapply(seq_len(max_len), function(i) {
    sapply(padded, `[`, i)
  })
  
  return(out)
}

paster <- function(vec){
  paste(head(vec,-1),collapse=tail(vec,1))%>%str2lang()%>%eval()
}

parse_vertical <- function(x) {
  
  # 1. Split each row into individual characters
  chars <- strsplit(x, "")
  
  max_len <- max(lengths(chars))
  
  # 2. Separate number rows and operator row
  num_rows <- chars[-length(chars)]
  op_row   <- chars[[length(chars)]]
  
  # 3. Transpose → list of columns, each column = vector of characters per row
  cols <- purrr::transpose(num_rows)
  
  # 4. For each column, collapse *vertically per number row*  
  #    but keep empty characters filtered out
  number_strings <- map(cols, function(col_vec){
    paste0(col_vec[col_vec != ""], collapse = "")
  })
  
  # 5. Operators align 1-to-1 with columns
  ops <- op_row[1:length(number_strings)]
  
  # Convert numeric strings to numbers
  numbers <- map_int(number_strings, as.integer)
  
  out<- tibble(number = numbers, op = ops)%>%
    mutate(grp = cumsum(is.na(number)))%>%
    group_split(grp)%>% 
    map(~ drop_na(.x, number) %>%select(-grp))
  
  #Same format as p.1 nums followed by operator.
  map(out, ~c(pull(.x,number),.x$op[[1]]))
  
}



part_1 <-function(x){
vecs <-x%>%parse_data()%>%rotate_list()
map_dbl(vecs, paster)%>%sum()
}


part_2 <- function(x) {
  vecs   <- parse_vertical(x)
  map_dbl(vecs, paster)%>%sum()
}

#Answers:
part_1(data) # 5667835681547
part_2(data) # 9434900032651
