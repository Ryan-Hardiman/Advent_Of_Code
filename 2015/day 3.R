library(tidyverse)
data <- clipr::read_clip()|>str_extract_all(".")|>unlist()

part1 <- function(old,new_sym){
  new <- case_when(
            new_sym == ">"~  tail(old,1)+1,
            new_sym == "<"~  tail(old,1)-1,
            new_sym == "^"~  tail(old,1)+complex(1,0,1),
            new_sym == "v"~  tail(old,1)-complex(1,0,1),
            )
  print(new)
  c(old,new)
  
  
}
#part1
locations <-reduce(data, part1, .init = complex(0,0))|>unique()|>length()
locations 

#part2
both <- matrix(data,nrow = 2)
santa <- both[1,]|>reduce(part1, .init = complex(0,0))
robo <- both[2,]|>reduce(part1, .init = complex(0,0))

c(santa,robo)|>unique()|>length()
