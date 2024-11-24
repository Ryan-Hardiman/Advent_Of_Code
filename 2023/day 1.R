library(tidyverse)
library(here)
library(stringr)
library(readxl)
source("get_aoc.R")
df<-get_aoc(1,2023)|>tibble()
names(df) <- "text"

#Part 1

df1<- df |> mutate(num1 = str_extract(string = text, pattern = "[0-9]"),
                   num2 = str_extract(string = text, pattern = "[0-9]{1}(?=[a-z]*$)"),
                   entered_num = paste0(num1, num2) |> as.numeric())|> select(entered_num) |> sum()

#Part 2

#possible word letters
"one|two|three|four|five|six|seven|eight|nine"

df2<- df |> mutate(num_1 = str_extract(string = text, pattern = "[0-9]{1}|one|two|three|four|five|six|seven|eight|nine"),
                   num_2 = stringi::stri_reverse(str_extract(string = stringi::stri_reverse(text), pattern = "[0-9]|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin")))




number_1_df <- tibble(values = rep(1:9,2)|>as.numeric(), num_1 = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", 1:9))
number_2_df <- tibble(values = rep(1:9,2)|>as.numeric(), num_2 = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", 1:9))
left_join(df2, number_1_df, by = c("num_1")) |> left_join( number_2_df, by = c("num_2")) |>mutate(entered_num = paste0(values.x, values.y)|>as.numeric()) |> select(entered_num)|>sum()
