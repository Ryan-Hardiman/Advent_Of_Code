source("get_aoc.R")
data <- get_aoc(17,2015)|>as.numeric()

perms <-expand.grid(map(data, ~c(.x,NA)))
#Part1
part1 <- perms |>janitor::adorn_totals(a, where = "col", ... = starts_with("Var"))|>filter(Total ==150)
nrow(part1)

#Part2
part2<- rowSums(!is.na(part1))
part2[part2 ==min(part2)]|>length()
