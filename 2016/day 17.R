source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(17,2016)

# ---- About ----
#
# Day 17 is a pathfinding problem set on a 4x4 grid.
# From the top-left corner (0+0i), we must reach the bottom-right (3+3i).
# 
# Doors (up, down, left, right) may be open or closed depending on the MD5 hash 
# of our current path string. Specifically:
#   - Take `MD5(input + path)`
#   - The first 4 characters correspond to doors U, D, L, R
#   - A door is **open** if its character is in [b-f]
#
# --- Part 1 ---
# Find the **shortest** path to reach (3+3i).
#
# --- Part 2 ---
# Find the **length** of the **longest** path that still reaches the goal.
#
# --- Strategy ---
# Use breadth-first search (BFS) to explore all valid paths.
#
# We'll:
#   - Represent position using complex numbers (e.g., 1+2i)
#   - Prune paths that move outside the grid
#
# Rather than tracking position state through each path step (pre-pruning),
# we compute the position from the path string as needed (post-pruning).



# ---- Grid Setup ----

# Movement directions, encoded as complex numbers
score <- c("U" =  0 - 1i,
           "D" =  0 + 1i,
           "L" = -1 + 0i,
           "R" =  1 + 0i)

moves <- c("U", "D", "L", "R")

# Grid positions range from:
#   0+0i (top-left) to 3+3i (bottom-right)
#
#     0+0i  1+0i  2+0i  3+0i
#     0+1i  1+1i  2+1i  3+1i
#     0+2i  1+2i  2+2i  3+2i
#     0+3i  1+3i  2+3i  3+3i


# ---- Path Expansion ----
# Given a character vector `xs` of path strings, generate all valid next moves.
# Each path expands into 0-4 new paths depending on which doors are open.
# We then discard any that move off the grid.

next_move <- function(xs) {
  # Compute MD5 hashes and extract first 4 chars for door state
  dirs <- cli::hash_md5(xs) %>%
    substr(1, 4) %>%
    str_split("") %>%
    map(~ moves[grepl("[b-f]", .x)])
  
  # Prune paths with no available directions
  valid_ix <- lengths(dirs) > 0
  if (!any(valid_ix)) return(tibble(path = character(0), pos = complex(0)))
  
  xs_valid <- xs[valid_ix]
  dirs_valid <- dirs[valid_ix]
  
  
  # Create all new paths from valid direction expansions
  new_paths <- map2(xs_valid, dirs_valid, paste0) %>% unlist()
  
  
  # Derive position from move sequence (excluding fixed input prefix)
  move_seqs <- substr(new_paths, nchar(data)+1, nchar(new_paths))
  steps_list <- str_split(move_seqs, "")
  positions <- map_vec(steps_list, ~ sum(score[.x]))
  
  # Prune paths outside grid bounds
  in_bounds <- Re(positions) >= 0 & Re(positions) <= 3 &
    Im(positions) >= 0 & Im(positions) <= 3
  
  tibble(path = new_paths[in_bounds],
         pos  = positions[in_bounds])
}  



# ---- Part 1: Shortest Path ----
# Use BFS to find the first path that reaches the goal (3+3i)
part_1 <-function(x){
  frontier <- tibble(path = x, pos = 0+0i)

  while(TRUE){ #Dangerous.
    next_frontier <- next_move(frontier$path)
    
    # Check for goal (3+3i)
    complete <- next_frontier %>% filter(pos == 3+3i)
    if (nrow(complete) > 0) {
      return(noquote(
        substr(complete$path[1], nchar(x)+1, nchar(complete$path[1]))
        ))
    }
    
    frontier <- next_frontier
  }
}

# ---- Part 2: Longest Path ----
# Explore the entire tree of possible paths, and keep track of the longest one
# that reaches the goal.

part_2 <-function(x){
  frontier <- tibble(path = x, pos = 0+0i)
  max_len <- 0
  
  while (nrow(frontier) > 0) {
    next_frontier <- next_move(frontier$path)
    
    # Separate complete and incomplete paths
    complete <- next_frontier %>% filter(pos == 3+3i)
    incomplete <- next_frontier %>% filter(pos != 3+3i)
    
    # Update maximum length
    if (nrow(complete) > 0) {
      max_len <- max(max_len, nchar(complete$path) - nchar(x))
    }
    
    frontier <- incomplete
  }
  
  return(max_len)
}

#Answers:
part_1(data) #DURLDRRDRD
part_2(data) #650
