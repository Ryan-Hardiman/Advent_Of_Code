get_aoc <- function(day, year, ck=session){
  httr::content(httr::GET(paste0('https://adventofcode.com/', year,'/day/', day, '/input'), 
                httr::set_cookies(session="test")
                ), as = "text")|>stringr::str_replace("\\n$","")|>stringr::str_split("\n")
  }

