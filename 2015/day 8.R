library(tidyverse)
library(stringi)
data <- clipr::read_clip()

#Part1
map_int(data, ~nchar(.x) - nchar(stri_unescape_unicode(.x))+2)|>sum()
#Part2
map_int(data, ~nchar(stri_escape_unicode(.x))+2 - nchar(.x))|>sum()
