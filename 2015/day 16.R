library(tidyverse)
to_match <- c("children: 3","cats: 7","samoyeds: 2","pomeranians: 3","akitas: 0","vizslas: 0", "goldfish: 5","trees: 3","cars: 2","perfumes: 1")

data <- clipr::read_clip()|>str_extract_all("\\w+: \\d+")
#part1
which(map(data, ~all(.x%in%to_match))==TRUE)

get_unique_in_data <- function(regy){
  data|>unlist()|>map(~.x|>str_extract(regy))|>unlist()|>na.omit()|>c()|>unique()|>sort()
}

new_match <- to_match[which(map_lgl(to_match,~is.na(str_extract(.x, "trees|cats|goldfish|pomeranians"))))]

get_nums <-function(data,critera,value){
  against <-to_match |>str_extract(glue::glue( deparse(substitute(critera)),".+"))|>str_extract("\\d+")|>as.numeric()|>na.omit()
  x <- data |>str_extract(glue::glue( deparse(substitute(critera)),".+"))|>str_extract("\\d+")|>as.numeric()|>na.omit()
  if(value == ">"){
    return(max((x>against)*1,0))
    
  }
  else{return(max((x<against)*1,0))}

}
part_2 <- map(data, ~(map(.x, \(y) y %in% new_match )|>unlist()*1)|>sum()
    +get_nums(.x, tree,">")
    +get_nums(.x, cat,">")
    +get_nums(.x, gold,"<")
    +get_nums(.x, pomer,"<")
    == length(.x)
    )

which(part_2 == TRUE)
