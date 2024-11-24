library(tidyverse)
data <- clipr::read_clip()
data<- data|>str_extract_all("^\\w+|(gain|lose)|\\d+|\\w+.$")|>tibble()|>unnest_wider(everything(), names_sep = "")
names(data)<- c("from","sign", "amount", "to")
data<- data |> mutate(to = str_extract(to,"\\w+"),
                      quant = ifelse(sign == "gain", as.numeric(amount), -as.numeric(amount)))|>select(-sign, -amount)


perms <- data %>%
  select(-quant)|>
  unlist() %>%
  unique() %>%
  combinat::permn()


calc_happyness <- function(x) {
  seq_x <- seq_along(x)
  len_x <- length(x)
  
  tibble(
    from = c(x, x),
    to = c(x[c(seq_along(x)[-1], 1)], x[c(length(x), seq_along(x)[-len_x])])
  ) |>
      left_join(data, by = c("from", "to")) %>%
      summarize(sum = sum(quant, na.rm = TRUE)) %>%
      pull(sum)
}
#part_1
persue_hapyness <- map(perms, calc_happyness)
max(unlist(persue_hapyness))

new_data <- rbind(data, tibble(from = c(rep("ryan", length(data$from|>unique())),data$from|>unique()),
                               to = c(data$from|>unique(),rep("ryan",length(data$from|>unique()))),
                               quant = 0))
new_perms <- new_data %>%
  select(-quant)|>
  unlist() %>%
  unique() %>%
  combinat::permn()

#part2
persue_hapyness <- map(new_perms, calc_happyness)
max(unlist(persue_hapyness))
