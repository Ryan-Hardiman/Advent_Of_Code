source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(20,2016)

#We hold data ranges as complex numbers, real = start, imaginary = duration. 
#We could avoid reduce in part 1 as our answer is the number following the first failing if-statement.
#Part 2 we build each of the collapsed ranges, sum up the duration and subtract this from the available space.

make_complex <- function(x){
  x|>str_match_all("\\d+")|>
    lapply(\(x){
      complex(1,x[1],as.numeric(x[2])-as.numeric(x[1]))
      })|>
    unlist() 
  } 

collect_runs <- function(acc, nxt, stop_no){
  inspect <- tail(acc,1)
  if(Re(inspect)+Im(inspect)>= Re(nxt)-1){ #Add to the duration, max avoids shrinking duration.
    acc[length(acc)] <- complex(1,Re(inspect),max(Im(inspect),Im(nxt)+Re(nxt)-Re(inspect)))   
    }else if(length(acc)!= stop_no) {
      acc <- c(acc,nxt) #Start next duration for part 2
      } 
  acc 
} 

part_1 <- function(x){   
  vals <- x|>make_complex()|>sort()  
  out <- reduce(vals, collect_runs,1,.init=0+0i)   
  Re(out)+Im(out)+1 
  } 

part_2 <- function(x,out_of){   
  vals <- x|>make_complex()|>sort()   
  outs<- reduce(vals,collect_runs,-1,.init=0+0i)+1i #+1i since we include the start IP.
  out_of - sum(Im(outs)) 
} 


#Answers:
part_1(data) # 32259706
part_2(data,2^32) # 113

