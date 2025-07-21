source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
library(memoise)
data <- get_aoc(14, 2016)

# Base hash function
base_hash <- function(s) cli::hash_md5(s)

# Stretched hash: apply MD5 2017 times (or once for part 1)
stretch_hash <- function(hash, rounds = 2016) {
  for (i in seq_len(rounds)) {
    hash <- cli::hash_md5(hash)
  }
  hash
}

# Memoised hash builders
hash_factory <- function(stretch = FALSE) {
  if (stretch) {
    memoise(function(s) stretch_hash(base_hash(s)))
  } else {
    memoise(base_hash)
  }
}

# Generate N hashes using chosen hash function
generate_hashes <- function(salt, max_index, hash_fn) {
  sapply(0:max_index, function(i) hash_fn(paste0(salt, i)))
}

# Find first triplet character in hash
find_triplet_char <- function(hash) {
  match <- str_match(hash, "(.)\\1\\1")
  if (is.na(match[1])) return(NA)
  match[2]
}

# Check for quintuple of a given char in hash
has_quintuple <- function(hash, char) {
  grepl(paste0(rep(char, 5), collapse = ""), hash)
}

# Solver for part 1 and 2
find_nth_key_index <- function(salt, n = 64, stretch = FALSE) {
  keys <- integer()
  index <- 0
  max_index <- 40000  # large enough for 64 keys I think... 
  hash_fn <- hash_factory(stretch)
  hashes <- generate_hashes(salt, max_index, hash_fn)
  
  while (length(keys) < n) {
    current_hash <- hashes[index + 1]
    triplet_char <- find_triplet_char(current_hash)
    
    if (!is.na(triplet_char)) {
      next_1000 <- hashes[(index + 2):(index + 1001)]
      if (any(map_lgl(next_1000, ~ has_quintuple(.x, triplet_char)))) {
        keys <- c(keys, index)
        cat("Found key", length(keys), "at index", index, "\n")#Debug / progress checking. 
      }
    }
    
    index <- index + 1
  }
  
  tail(keys, 1)  # Return the nth key's index
}



part_1 <- function(x){
  find_nth_key_index(x)
}

part_2 <- function(x){
  find_nth_key_index(x,64,T)
}


#Answers:
part_1(data) # 25427 
part_2(data) # 22045
