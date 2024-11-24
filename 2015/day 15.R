library(tidyverse)
data <- clipr::read_clip()|>str_extract_all("[-]*\\d+")|>tibble()|>unnest_wider(everything(),names_sep = "_")
names(data) <- c("cap", "dur", "fla", "tex", "cal")
data <- data |>select(-cal)|>mutate(across(everything(), as.numeric))|>t()
data

#Must make 1 of each else the product is 0. 
make_perms <- function(n){
  
  df<- expand_grid(1:n,1:n,1:n,1:n)
  df <- df |>filter(rowSums(df)==n)|>as.list()|>c()
  df}

ary <- make_perms(100)

test <- function(x){
   sweep(data, 2,unlist(x),"*" )
}
tst <- list(W =ary$W|>head(10),X = ary$X|>head(10),Y =ary$Y|>head(10),Z = ary$Z|>head(10))

part_1 <- pmap(list(ary[[1]],ary[[2]],ary[[3]],ary[[4]]), \(w,x,y,z) sweep(data, 2,c(w,x,y,z) , "*")|>rowSums()|>pmax(0)|>prod())
part_1 |>unlist()|>max()

calories <- clipr::read_clip()|>str_extract("\\d+$")|>as.numeric()|>tibble()
names(calories ) <- "cal"
calories <- calories|>t()
calory_list <- pmap(list(ary[[1]],ary[[2]],ary[[3]],ary[[4]]), \(w,x,y,z) sweep(calories, 2,c(w,x,y,z) , "*")|>rowSums())


part_2 <- part_1[which(calory_list == 500)]|>unlist()|>max()
part_2


