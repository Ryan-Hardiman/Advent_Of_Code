new_day <- function(day = lubridate::today()|>lubridate::day(),
                    year=lubridate::today()|>lubridate::year()){
  out_path <- glue::glue("{year}/day {day}.R")
  if(file.exists(here::here(out_path))){stop("ALREADY EXISTS")}
  temp<-whisker::whisker.render(readr::read_file("template.R"), data = list(day = day, year = year))
  
  readr::write_file(temp, file = out_path)
  source(out_path)
  rstudioapi::documentOpen(path=out_path)
  }
  