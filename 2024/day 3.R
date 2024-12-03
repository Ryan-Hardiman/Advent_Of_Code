source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(3,2024)


part_1 <-function(x){
x|>str_extract_all("mul\\(\\d{1,3},\\d{1,3}\\)")|>
    unlist()|>str_extract_all("\\d+")|>
    map(~.x |>as.numeric()|>prod())|>unlist()|>sum()
}


part_2 <-function(x){
x|>paste0(collapse="")|>
    str_remove_all("don't\\(\\).*?do\\(\\)|don't\\(\\).*?(?!do\\(\\))$")|>
    str_extract_all("mul\\(\\d{1,3},\\d{1,3}\\)")|>
    unlist()|>str_extract_all("\\d+")|>
    map(~.x |>as.numeric()|>prod())|>unlist()|>sum()
  }


#Answers:
part_1(data) # 187833789
part_2(data) # 94455185

