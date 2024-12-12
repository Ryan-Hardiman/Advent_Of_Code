source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required 
data<- get_aoc(12,2024)

#My code collects the points of each subgroup in complex form
#The area is simply the number of points in each group
#To get the perimeter, do 4 less the number of points touching, for every cell in
#each group in turn.

#Part 2 involves the area again, but now we need the number of edges, 
#Remember that the number of edges is the number of vertices
#So we solve this instead, count vertices where they occur examples below.

#x represents our current pointer, o's are part of our subgroup and " " is void.

# Dealing with Neighbours = 1:   
# X Should return 2
# xo
# 

# Dealing with Neighbours = 2:   
# X Should return 2  # X Should return 1  # X Should return 0
# xo                 # xo                 # oxo                     
# o                  # oo                 #       

# Dealing with Neighbours = 3:       
# X Should return 2  # X Should return 1  # X Should return 0                                  
#  o                 #                    # 
# oxo                # oo                 # ooo     
#                    # oxo                # oxo    

# Dealing with Neighbours = 4:
# X Should return 1
# ooo
# oxo
# oo

make_graph <- function(x){ 
  len <- nchar(x[[1]]) 
  x<- x|> str_extract_all(".")|>unlist() 
  chars <<-unique(x) # double insert 
  graph <<-x|>matrix(ncol =len) # double insert 
  } 

get_char_locs <- function(chars){ 
  map(chars, ~which(graph==.x, arr.ind = TRUE))|>
    map(~.x[,1]+.x[,2]*1i) #compress grid to complex vector. 
  } 

get_neighbours <-function(xy, char_space){ 
  if(length(xy)==length(char_space)){return(list(xy))} # all consumed 
  
  
  neighbour <- map(xy, ~char_space[which(abs(char_space - .x) ==1)])|>unlist()|>unique() 
  #Required to not find neighbours already known
  if(any(neighbour %in% xy)){neighbour <- neighbour[-which(neighbour%in%xy)]} 
  if(length(neighbour)==0){return(xy)}#consumed all, return subgroup. 
  
  get_neighbours(c(xy, neighbour),char_space) 
} 

#Recursively get the next available (not yet-sub-grouped) item and its neighbours. 
#Add subgroup to subgroup list and remove from available material until void remain.
collect_char_groups <- function(available, new = available[[1]], complete= c()){
  tmp <- get_neighbours(new,available) 
  complete<- c(complete,list(tmp)) 
  available <- available[-which(available%in%tmp)] 
  
  if(length(available)==0){return(complete)} 
  
  new<-available[[1]] 
  collect_char_groups(available, new,complete) 
  } 

get_area <- function(x){length(unlist(x))} #As on tin

get_perimeter <- function(x){map_dbl(x,~4-sum(abs(x - .x)==1))|>sum()} 



part_1 <-function(x){ 
  make_graph(x) 
  char_locations <- get_char_locs(chars) 
  char_groups <- map(char_locations,collect_char_groups)|>list_flatten() 
  area <- map(char_groups, get_area) 
  perimeter <- map(char_groups, ~get_perimeter(unlist(.x))) 
  map2_dbl(area,perimeter, prod)|>sum() 
  } 





corner_case_for_2 <- function(x,neighbours, char_group){ # U G L Y
  if((Re(neighbours)|>unique()|>length()==1 | Im(neighbours)|>unique()|>length()==1)){return(0)} 
  3- length(
    which(
      (unique(Re(c(x,neighbours)))+
         1i*unique(Im(c(x,neighbours)))
       )%in%char_group
      )
    )
  } 

corner_case_for_3 <- function(x, neighbours, char_group) {# U G L Y
  max(0, 2 -
        sum(
          (unique(Re(neighbours))[unique(Re(neighbours)) != Re(x)] +
          1i * unique(Im(neighbours))[unique(Im(neighbours)) != Im(x)]
          ) %in% char_group)
      )
} 

corner_case_for_4 <- function(x,char_group){
  max(4- sum((c(x+(1+1i),x+(1-1i),x-(1+1i),x-(1-1i))%in%char_group)),0)
}

is_vertice <- function(x,char_group){ 
  if(length(char_group)==1){return(4)} # A single point has no neighbours but requires 4 vertices.
  neighbours <- char_group[which(abs(char_group-x)==1)]
  if(length(neighbours)==4){return(corner_case_for_4(x,char_group))} #Four neighbours may add 1. in a + with corner exposed.
  if(length(neighbours)==3){return(corner_case_for_3(x,neighbours,char_group))}#Three neighbours may add 1 or two vertices.
  if(length(neighbours)==2){return(corner_case_for_2(x,neighbours,char_group))}#two neighbours can add either 0, 1, or 2 vertices (e.g L shape)
  return(2) #A single neighbour adds two vertices
} 

count_vertices<- function(x){map_dbl(x,~is_vertice(.x, x)) } 



part_2 <-function(x){ make_graph(x) 
  char_locations <- get_char_locs(chars) 
  char_groups <- map(char_locations,collect_char_groups)|>list_flatten() 
  area <- map(char_groups, get_area) 
  vertices <- map(char_groups, ~count_vertices(unlist(.x))|>unlist()|>sum()) 
  map2_dbl(area,vertices, prod)|>sum() 
} 


#Answers:
part_1(data) #1371306 
part_2(data) #805880

