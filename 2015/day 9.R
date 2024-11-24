library(tidyverse)
data <- clipr::read_clip()|>tibble()|>separate_wider_delim(cols = everything(),delim = " = ", names = c("dest", "dist"))|>separate_wider_delim(cols = "dest",delim = " to ", names = c("from", "to"))

perms <- data %>%
  select(-dist)|>
  unlist() %>%
  unique() %>%
  combinat::permn()


both_ways <- rbind(data, data|>select(dist, to=from, from=to))|>mutate(dist = as.numeric(dist))

calc_distance <- function(x) {
  tibble(
    from = x[-length(x)],
    to = x[-1]
  ) %>%
    left_join(both_ways, by = c("from", "to")) %>%
    summarise(sum = sum(dist)) %>%
    pull(sum)
}
#part_1
map_int(perms, calc_distance) %>%
  min()


#part_1
map_int(perms, calc_distance) %>%
  max()

