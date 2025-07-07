source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(9,2016)

consume <- function(current, remain, recursive = FALSE) {
  if (nchar(remain) == 0) return(current)
  
  #Regex match on groups: 
  #1st element = full match
  #2nd element = anything before bracket "("
  #3rd element = first number within bracket
  #4th element = second number within bracket
  m <- str_match(remain, "^(.*?)\\((\\d+)x(\\d+)\\)")%>%first()
  
  if (is.na(m[1])) {
    # No more markers  - We're done
    return(current + nchar(remain))
  }
  
  # Pre-marker literal text
  literal <- m[2]
  len <- as.numeric(m[3])
  times <- as.numeric(m[4])
  
  current <- current + nchar(literal)
  
  # Extract segment to repeat
  marker_len <- nchar(m[1])
  to_repeat <- substr(remain, marker_len + 1,  marker_len + len )
  rest <- substr(remain, marker_len + len + 1, nchar(remain))
  
  #Assuming first that brackets in "to_repeat" will not "leak" outside of to_repeat..
  added <- if (recursive) {
    as.numeric(consume(0, to_repeat, recursive = TRUE)) * times
  } else {
    nchar(to_repeat) * times
  }
  
  current <- current + added
  Tailcall(consume, current, rest, recursive)
}

part_1 <- function(x) {
  consume(0, x, recursive = FALSE)
}

part_2 <- function(x) {
  consume(0, x, recursive = TRUE)
}

#Answers:
part_1(data) #123908
part_2(data) #10755693147
