source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(03,2025)

parse_data <- function(x){
  x%>%str_split("")%>%lapply(as.numeric)
}

get_joltage <- function(bank, bank_len){
  if(bank_len == 0){return(0)}
  new_len <- bank_len-1
  battery <- bank[1:(length(bank)-new_len)]%>%max() #Get sthe 
  return(10^(new_len)*battery + #Recursively multiply joltage by 10 and add new battery.
           get_joltage( 
             bank[-(1:(match(battery,bank)))], #Chop off invalid banks (before + on new battery).
             new_len)
         )
}


part_1 <-function(x){
x%>%parse_data()%>%map_dbl(get_joltage,2)%>%sum()
}


part_2 <-function(x){
  x%>%parse_data()%>%map_dbl(get_joltage,12)%>%sum()
}

#Answers:
part_1(data) # 17430
part_2(data) # 171975854269367



#First attempt at part 1 below:
data|>str_extract_all("\\d")|>
  lapply(as.numeric)|>
  lapply(rle)|> #RLE ro remove repeated successive numbers, drastically shortening run time.
  map_dbl( \(x) map(
    1:length(x$values), #Makes a triangle 
    \(y) accumulate(x$values |> tail(-y), \(acc, nxt) x$values[y] *
                      10 + nxt, .init = 0)
  ) |> unlist() |> max()) |> sum()

