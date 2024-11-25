source("get_aoc.R")
library(tidyverse)
library(stringi)
library(rlang)
library(memoise)
data <-get_aoc(7,2015)


fns <- data |>str_extract("\\w+$")
vars <- data |>str_extract("(?:(?! ->).)+")|>str_extract_all("[^A-Z ]+")|>map(~.x|>gsub(pat ="(.+)",rep ="`\\1`"))
operator <- data|>str_extract("[A-Z]+")|>replace_na("SET")

#Binary Operators 
NOT    <- function(myx){2^myx%%2}
AND    <- function(myx,myy){((myx+myy)==2)*1}
OR     <- function(myx,myy){((myx+myy)>=1)*1}
RSHIFT <- function(myx,quant){myx|>lag(to_int(quant))|>replace_na(0)}
LSHIFT <- function(myx,quant){myx|>lead(to_int(quant))|>replace_na(0)}
SET    <- function(myx){myx}

#Conversions
to_binary <- function(myx){rev(1*as.logical(intToBits(myx)[1:16]))}
to_int <- function(myx){(myx*2^(16:1-1))|>sum()}
#Examples
#to_binary(65535)
#to_binary(1)
#to_int(to_binary(12345))

mem_eval_tidy <- memoise(function(xyz){print(xyz);eval_tidy(xyz)})
fn_factory <- function(name,vars,op){

  eval(parse(text =
               paste0(
                 "delayedAssign(\"",name ,"\",quote(",op,"(",glue::glue_collapse(paste0("mem_eval_tidy(",vars,")"),sep = ","),")), assign.env = .GlobalEnv)"
               )
  ))

}

make_nums <- function(vars){
  vars|>unlist()|>str_extract("`\\d+`")|>unique()|>na.omit()|>walk(\(x) eval(parse(text = paste0(x, "<<-to_binary(", str_extract(x,"\\d+"),")"))))
}

make_nums(vars)

pwalk(list(fns, vars, operator),fn_factory)



#Part 1 #16076
part_1<-mem_eval_tidy(a)|>to_int()
part_1



rm(list =setdiff(ls(),c("OR", "SET","AND","LSHIFT", "RSHIFT","NOT", "fns", "vars", "operator","fn_factory", "get_aoc","data", "mem_eval_tidy","make_nums","to_binary","to_int")))
#Add new data in
data <- c(data,"16076 -> b")
#Repeat above
fns <- data |>str_extract("\\w+$")
vars <- data |>str_extract("(?:(?! ->).)+")|>str_extract_all("[^A-Z ]+")|>map(~.x|>gsub(pat ="(.+)",rep ="`\\1`"))
operator <- data|>str_extract("[A-Z]+")|>replace_na("SET")
vars|>unlist()|>str_extract("`\\d+`")|>unique()|>na.omit()|>walk(make_nums)
pwalk(list(fns, vars, operator),fn_factory)

#Need to re-create memoised function 
mem_eval_tidy <- memoise(function(xyz){print(xyz);eval_tidy(xyz)})



#Part 2 #2797
part_2<-mem_eval_tidy(a)|>to_int()
part_2


#microbenchmark::microbenchmark(mem_eval_tidy(a))
# Unit: microseconds
# expr  min    lq   mean median   uq     max neval
# mem_eval_tidy(a) 46.7 47.85 843.99   48.6 49.5 79421.6   100
