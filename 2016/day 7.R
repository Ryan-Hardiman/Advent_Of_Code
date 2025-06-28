source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(7,2016)

parse_data <- function(x){
  outer <- x%>% str_split("\\[[^\\[]*\\]")
  inner <- x%>% str_extract_all("\\[[^\\[]*\\]")%>%map(~gsub("^.|.$","",.x))
  list(outer,inner)
}

inner_fn <- function(x){
   x%>%str_extract_all(".")%>%
     map( ~.x%>%
         unique()%>%
         length()%>%
         lapply("==",2)
         )%>%
     unlist()%>%
     any()
}

abba <- function(x){
  x%>%str_extract_all("(.)([^\\1\\[\\]])\\2\\1" )
}

#str_extract_all doesnt permit overlapping matches so bring out the big guns..
aba <- function(x){
  x%>%stri_match_all_regex("(?=(([a-z])(?!\\2)[a-z]\\2))")%>%
    map(~.x[,2]%>%na.omit)
}


part_1 <-function(x){
  inner_outer <- x%>%parse_data()%>%map(abba)
   map2_lgl(
     inner_outer[[1]],
     inner_outer[[2]],
     ~!inner_fn(.y)&inner_fn(.x) )%>%
     sum()
}


part_2 <-function(x){
  inner_outer <- x%>%parse_data()%>%map(aba)
  map2_lgl(
    inner_outer[[1]],
    inner_outer[[2]],
    ~map_lgl(.x, \(a) gsub("(.)(.)\\1","\\2\\1\\2",a) %in% .y)%>%any())%>%
    sum()
}

#Answers:
part_1(data) #115
part_2(data) #231  

