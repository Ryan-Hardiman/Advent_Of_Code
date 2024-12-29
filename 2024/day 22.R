source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(22,2024)

to_loop <- function(x){
  x <- bitwXor(x, (x * 64) %% 16777216) 
  x <- bitwXor(x, x %/% 32)
  bitwXor(x, (x * 2048) %% 16777216)
}

part_1 <- function(x){
  x <- as.numeric(x)
  for(i in 1:2000){
    x<-to_loop(x)
  }
  sum(x)
}

#This feels like its more easily done in excel. 

part_2 <-function(x){
  x <- as.numeric(x)
  digits <- matrix(0,2001,ncol=length(x))
  normalise_diffs <- matrix(0,2000,ncol=length(x))
  cumdiffs <- matrix(0,1997,ncol=length(x))
  for(i in 1:2000){
    x<-to_loop(x)
    digits[i+1,]<-x%%10
    #Make -9 --> 1, -8-->2 etc. up to 20 
    #required later so the diff sums at any stage is unique,
    #using powers to separate rows.
    normalise_diffs[i,]<- 10 + digits[i+1,] - digits[i,] 
    if(i>3) cumdiffs[i-3,] <- colSums(normalise_diffs[i - 0:3,]*20^(0:3))    
  }
  cumdiffs
  
  #We now sum (over the each of the 2000 sequence) each ocurrence of the **unique**
  #Cumsum multiplied by the number of bananas (1*x + 1*y + 1*z + ... = end) and
  #then take the maximum of this list. 
  possible <- rep(0,max(cumdiffs)) 
  for (i in seq_along(x)){
  possible[cumdiffs[!duplicated(cumdiffs[,i]),i]] <-  
    possible[cumdiffs[!duplicated(cumdiffs[,i]),i]] + 
    digits[-(1:4),i][!duplicated(cumdiffs[,i])]
  }
  max(possible)
}

#Answers:
part_1(data) # 19847565303
part_2(data) # 2250

