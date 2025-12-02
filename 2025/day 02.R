source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(02,2025)

# We replace - with : to utilise R's numeric vector creation.
#str2lang gives a "call" which we execute by using eval.
parse_data <- function(x){
  x%>%str_replace_all("-",":")%>%
    str_extract_all("\\d+:\\d+")%>%
    unlist()%>%
    map(~.x%>%str2lang()%>%eval()%>%as.character())%>%
    unlist()%>%
    as.character()
}

#Part 1 requires us to check the first half of the string matches the second. 
# With numbers of odd length being prefixed with 0's 
get_mirror_copy <- function(x){
string_length <- nchar(x)

midpoint <- floor(string_length/2)

if(string_length %%2 ==0){
  out <- paste0(
    substr(x,midpoint+1,string_length),
    substr(x,1,midpoint)
    )
}else{
  out <-paste0(
    0,
    substr(x,midpoint+2,string_length),
    substr(x,midpoint+1,midpoint+1),
    substr(x,1,midpoint)
  )
}

out
}

#We want to split the string into all the possible divisors of current length
#We then check if repeating the first part "n" times gives the original string.
pattern_divisiors <- function(x){
  string_length <- nchar(x)
  divisors <- numbers::divisors(string_length)
  divisors <- divisors[divisors != string_length]
  
 map_lgl(divisors, \(div){
    block <- substr(x,1,div)
    reps <- paste0(rep(block,string_length/div),collapse = "")
    #debug comment
    #if(reps == x) print(paste0(block, " match ",x)) 
    reps == x
  } )%>%any()
}


part_1 <-function(x){
  ids <- parse_data(x)
  mirror_id <- map_chr(ids,get_mirror_copy)
  invalid <- ids[which(ids ==mirror_id)]
  
  map_dbl(invalid, as.numeric)%>%sum()
}

#There is a way of reducing the input into pattern_divisors, as we already know
# some of the invalid codes (and so we can skip them). But that would require 
# an "%in%" which would slow this down slightly. 
part_2 <-function(x){
  ids <- parse_data(x)
  mirror_id <- map_chr(ids,get_mirror_copy)
  
  pattern_valid <- map_lgl(ids, pattern_divisiors)
  
  invalid <- ids[which(ids ==mirror_id | pattern_valid)]
  
  map_dbl(invalid, as.numeric)%>%sum()
}

#Answers:
part_1(data) # 24747430309
part_2(data) # 30962646823
