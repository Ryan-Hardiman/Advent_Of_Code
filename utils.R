new_day <- function(day = lubridate::today()|>lubridate::day(),
                    year=lubridate::today()|>lubridate::year()){
  out_path <- glue::glue("{year}/day {stringr::str_pad(day, width = 2, pad = '0')}.R")
  if(file.exists(here::here(out_path))){stop("ALREADY EXISTS")}
  temp<-whisker::whisker.render(readr::read_file("template.R"), data = list(day = stringr::str_pad(day, width = 2, pad = "0"), year = year))
  
  readr::write_file(temp, file = out_path)
  source(out_path)
  rstudioapi::documentOpen(path=out_path)
  }
  
