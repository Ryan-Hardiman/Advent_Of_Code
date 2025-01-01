source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
library(igraph) #Some heavy lifting..
data<- get_aoc(23,2024)
tst <- clipr::read_clip()

parse_data <- function(x){
  x|>str_extract_all("\\w+")|>
    unlist()|>
    matrix(ncol = 2, byrow = TRUE)
}

part_1 <-function(x){
  x <- parse_data(x)
  graph <- graph_from_edgelist(x,directed =F)
  grp3 <- cliques(graph, max=3,min=3)
  grp3_names <- map(grp3, ~.x|>_$name)
  map_lgl(grp3_names, ~(.x|>str_extract_all("^t")|>unlist()|>length()>0))|>sum()
}


part_2 <-function(x){
  x <- parse_data(x)
  graph <- graph_from_edgelist(x,directed =F)
  largest_cliques(graph)|>_[[1]]|>_$name|>sort()|>paste0(collapse=",")|>noquote()
}

#Answers:
part_1(data) # 1149
part_2(data) # as,co,do,kh,km,mc,np,nt,un,uq,wc,wz,yo

