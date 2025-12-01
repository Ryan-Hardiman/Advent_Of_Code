source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(01,2025)

#We're told the inital state of the dial.
dial <- c(50:99,0:49)

#Consider turning Left as negative and right as positive.
parse_data <- function(x){
  x%>%
    str_replace_all("L","-")%>%
    str_remove_all("R")%>%
    as.numeric() 
}


#Simple vector rearrangement. 
rotate <- function(dial,amt){
  c(tail(dial,-amt),head(dial,amt))
}

check_rotation <- function(dial,amt){
  if(amt < 0)
    values <-tail(dial, abs(amt) %% 100)
  else
    values <-head(dial, abs(amt) %% 100)

 
  entire_rotations <- floor(abs(amt)/100)
  #Need to exclude starting at 0, hence remove the first item. 
  partial_rotation_match <- 0 %in% tail(values,-1)
  entire_rotations + partial_rotation_match
}



part_1 <-function(x){

  #Get the rotations.
  rotations <- x%>%parse_data()
  
  #Recursive function to count the number of 0's occurring.
  password <- reduce(rotations, 
                     \(acc, nxt){
                     #Get the next rotation
                     new <- rotate(acc[[1]],nxt %%100)
                     password <- acc[[2]]+(head(new,1)==0)
                     #output at each level is the new rotation and the count.
                     list(new,password)
                     }
                     ,.init = list(dial,0))
  #Returning the count 
  password[[2]]
}



part_2 <-function(x){
  
  #Get the rotations.
  rotations <- x%>%parse_data()
  
  #Recursive function to count the number of 0's occurring.
  password <- reduce(rotations, 
                     \(acc, nxt){
                       #Get the next rotation
                       new <- rotate(acc[[1]],nxt %%100)
                       passed <- check_rotation(acc[[1]],nxt)
                       landed_on <- 1*(head(new,1)==0)
                       password <- acc[[2]]+ passed + landed_on
                       #Debug using easy print statements
                       #print(paste0("Started on ", head(acc[[1]],1), " moved ", nxt, " landed ", new[[1]], " passed ",passed ))
                       list(new,password)
                     }
                     ,.init = list(dial,0))
  #Returning the count 
  password[[2]]
}

#Answers:
part_1(data) #962
part_2(data) # 5782
