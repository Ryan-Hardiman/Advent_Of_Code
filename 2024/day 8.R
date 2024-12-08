source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
library(unpivotr)
data<- get_aoc(8,2024)
tst<-clipr::read_clip()

cell_data <- function(x){
  x|>str_extract_all("")|>
    tibble()|>magrittr::set_colnames("data")|>
    unnest_wider(data,"")|>as_cells()
}

#Explanation:
#We gather the list of congruent symbols 
#in an X,Y form represented as complex value.
#We gather the permutations of any congruent symbol pair
#and add the difference to both pairs, once for part1
#and as many as required for part 2.

get_antinodes <- function(x,part_2=FALSE){
  grid <- cell_data(x)
  max_col <- max(grid$col)
  max_row <- max(grid$row)
  steps <- ifelse(part_2,max(max_row,max_col),1)
  nodes <- grid|>filter(chr !=".")|>
    mutate(pos = row + col*1i)|>
    group_split(chr)|>
    map(~.x|>pull("pos")|>combn(2))
  anti_nodes <-nodes|>map(\(a) mapply(\(x,y) c(x,y)+ ceiling(seq(0.5-min(2,steps)+1,steps,by=0.5))*c(x-y, y-x),a[1,],a[2,] ))|>unlist()
  anti_nodes[which(Re(anti_nodes)>0 &
                     Im(anti_nodes)>0 &
                     Re(anti_nodes)<=max_row &
                     Im(anti_nodes)<=max_col)]|>unique()|>length()
}


part_1 <-function(x){
  get_antinodes(x)
}

part_2 <-function(x){
 get_antinodes(x,TRUE)
}

#Answers:
part_1(data) #278
part_2(data) #1067
