library(tidyverse)
data <- get_aoc(15,2016)|>str_match_all("\\d+")|>lapply(parse_number)


solver <- function(x,p2=F){
if(p2) x <- append(x,list(c(0,11,0,0)))
  
mdl <- x|>lapply(nth,2)|>unlist()
cur <- x|>lapply(nth,4)|>unlist()
cur <- cur + 1:length(cur)-1
print(cur)
i<-0
while(!all(cur == 0)){
  cur<- (cur+1)%%mdl
  print(cur)
  i<-i+1
}
i-1
}

#Part 1: 
solver(data,F) # 400589

#Part 2: Slow but works. 
solver(data,T) # 3045959
