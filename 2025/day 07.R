source("get_aoc.R")
library(tidyverse)

data <- get_aoc(7, 2025)

# Parse input into logical vectors:
# TRUE for S or ^, FALSE otherwise
parse_data <- function(x) {
  str_split(x, "") |> map(`%in%`, c("S", "^"))
}

step_day7 <- function(acc, row) {
  
  timelines <- acc$timelines
  has_beam  <- timelines > 0
  
  to_split <- has_beam & row
  to_fall  <- has_beam & !row
  
  # Part 1: count activated splitters
  acc$splits <- acc$splits + sum(to_split)
  
  # Part 2: propagate timelines
  split_mass <- timelines * to_split
  
  acc$timelines <-
    timelines * to_fall +
    lag(split_mass,  default = 0) +
    lead(split_mass, default = 0)
  
  acc
}

solve_day7 <- function(x) {
  rows <- parse_data(x)
  
  init <- list(
    timelines = as.numeric(rows[[1]]),  # S = 1, rest = 0
    splits    = 0
  )
  
  reduce(rows[-1], step_day7, .init = init)
}

out <- solve_day7(data)

# Answers
out$splits          # Part 1: 1642
sum(out$timelines)  # Part 2: 47274292756692

microbenchmark::microbenchmark(
  solve_day7(data),
  times = 50
)
#Unit: milliseconds
#expr                     min                      lq                    mean                  median
#solve_day7(data) 7.115600999999999842771 7.206400999999999612555 7.698076819999999820254 7.276151000000000479417
#uq                     max neval
#7.434401000000000259149 16.86870100000000149976    50
