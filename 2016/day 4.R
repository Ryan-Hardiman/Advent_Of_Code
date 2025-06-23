source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(4,2016)

get_id <- function(x){
  x%>%str_extract("\\d+")%>%as.numeric()
}

is_valid_room <- function(room) {
  # Extract the encrypted name, sector ID, and checksum
  encrypted_name <- str_extract(room, "^[a-z-]+")
  checksum <- str_extract(room, "(?<=\\[)[a-z]+(?=\\])")
  
  # Remove dashes and count letters
  letters_only <- str_remove_all(encrypted_name, "-") %>% str_split("", simplify = TRUE)
  freq_table <- table(letters_only)
  
  # Sort by frequency (descending), then alphabetically
  sorted_letters <- names(sort(freq_table, decreasing = TRUE))
  freqs <- sort(unique(freq_table), decreasing = TRUE)
  
  ordered <- unlist(
    lapply(freqs, function(f) {
      letters_with_f <- names(freq_table[freq_table == f])
      sort(letters_with_f)  # alphabetical for ties
    })
  )
  
  # Create expected checksum from first 5 letters
  expected_checksum <- paste0(ordered[1:5], collapse = "")
  
  return(expected_checksum == checksum)
}


part_1 <-function(x){
x[map_lgl(x,is_valid_room)]%>%get_id()%>%sum()
}

#Surprising turn, well now I guess we're shifting letters mod 26.
decrypt_name <- function(encrypted_name, sector_id) {
  rotate <- function(char, shift) {
    if (char == "-") return(" ")
    shifted <- ((utf8ToInt(char) - utf8ToInt("a") + shift) %% 26) + utf8ToInt("a")
    intToUtf8(shifted)
  }
  
  strsplit(encrypted_name, "")[[1]] |>
    sapply(rotate, shift = sector_id) |>
    paste0(collapse = "")
}

part_2 <- function(x) {
  encrypted_name <- str_extract(x, "^[a-z-]+")
  sector_id <- as.numeric(str_extract(x, "\\d+"))
  decrypted <- map2(encrypted_name, sector_id, decrypt_name)
  sector_id[decrypted%>%grepl(pattern="north")]
}


#Answers:
part_1(data) #278221
part_2(data) #267
