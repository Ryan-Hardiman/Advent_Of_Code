library(tidyverse)
data <- clipr::read_clip()

#Part1
first_condition <- data |>str_extract(".*[aeiou]{1}.*[aeiou]{1}.*[aeiou]{1}")
second_condition <- data |> str_extract("([a-z])\\1")
third_condition <- data|>str_extract("ab|cd|pq|xy")|>is.na()
which(!is.na(first_condition) & !is.na(second_condition) & third_condition)|>length()

#Part2
cond_1 <- data |>str_extract("([a-z][a-z]).*\\1")
cond_2 <- data |>str_extract("([a-z]).{1}\\1")
which(!is.na(cond_1) & !is.na(cond_2))|>length()
