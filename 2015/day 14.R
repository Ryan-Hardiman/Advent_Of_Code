library(tidyverse)
data <- clipr::read_clip()|>str_extract_all("^\\w+|\\d+")|>tibble()|>unnest_wider(everything(),names_sep = "_")
names(data) <- c("deer", "spd", "dur", "slp" )
data <- data|>mutate(across(-deer, as.numeric),dist=0)

duration <-2503

part_1 <- data|>mutate(dist = floor(duration/(dur + slp))*spd*dur + pmin(dur, (dur + slp) %% duration)*spd)|>select(dist)|>max()
part_1

time <- 0:(duration-1) + 1

data$score <- 0

next_second <- function(acc, nxt){
  acc<- acc |>mutate(dist = pmap_dbl(list(dur,dist,slp,spd), \(dur, dist,slp,spd) dist + ifelse((nxt%%(dur+slp)) %in% c(1:dur) , spd,0)),
               score = score + ifelse(dist == max(dist),1,0))
print(acc)
acc
  }
#Part2
reduce(time, next_second, .init = data)|>select(score)|>max()

