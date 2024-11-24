library(tidyverse)
data <- clipr::read_clip()

part1 <- data |>str_extract_all("[-]*\\d+")|>unlist()|>as.numeric()|>sum()

reds <- data |> str_extract_all("\\{[^\\[\\]]*?red[^\\[\\]].?\\}")#|>lapply(str_extract_all,"[-]*\\d+")|>unlist() |>as.numeric()|>sum()
part1 - reds

#part2 


check_json<- function(x) {
  if(length(x) > 1 | is.list(x)) return(TRUE)
  x!="red"
}

parse_red <- function(input) {
  repeat {
    end <- str_locate(input, "\\}")[[1]][[1]]
    
    if (all(is.na(end))) break
    
    starts <- str_locate_all(input, "\\{")[[1]][, 1]
    start <- max(starts[starts < end])
    
    reduced <- str_sub(input, start, end)
    
    json <- jsonlite::parse_json(reduced)
    
    if (all(map_lgl(json, check_json))) {
      out <- str_extract_all(reduced, "-*[0-9]+")[[1]]
      out <- sum(as.numeric(out), na.rm = TRUE)
    } else {
      out <- 0
    }
    
    str_sub(input, start, end) <- out
  }
  
  out <- str_extract_all(input, "-*[0-9]+")[[1]]|>c()|>as.numeric()|>sum()
  out
}
#part_2
parse_red(data)
