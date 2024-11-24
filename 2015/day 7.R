source("get_aoc.R")
library(tidyverse)
library(stringi)
data <- get_aoc(7,2015)

fns <- data |>str_extract("\\w+$")#|>map(~.x|>gsub(pat ="(.+)",rep ="`\\1`"))
vars <- data |>str_extract("(?:(?! ->).)+")|>str_extract_all("[^A-Z ]+")|>map(~.x|>gsub(pat ="(.+)",rep ="`\\1`"))
operator <- data|>str_extract("[A-Z]+")|>replace_na("SET")

#Binary Operators
NOT <- function(myx){2^myx%%2}
AND <- function(myx,myy){((myx+myy)==2)*1}
OR  <- function(myx,myy){((myx+myy)>=1)*1}
RSHIFT <- function(myx,quant){myx|>lag(as.numeric(quant))|>replace_na(0)}
LSHIFT <- function(myx,quant){myx|>lead(as.numeric(quant))|>replace_na(0)}
SET <- function(myx){if(length(myx)==1){as.numeric(myx)|>to_binary()}else{myx}}

#Conversions
to_binary <- function(myx){rev(1*as.logical(intToBits(myx)[1:16]))}
to_int <- function(myx){(myx*2^(16:1-1))|>sum()}
#Examples
#to_binary(65535)
#to_binary(1)
#to_int(to_binary(12345))

fn_fatory <- function(name,vars,op){

  eval(parse(text = 
               paste0(
                 "delayedAssign(\"",name ,"\",",op,"(",glue::glue_collapse(vars,sep = ","),"), assign.env = .GlobalEnv)"
                 )
             ))
    
}

pwalk(list(fns, vars, operator),fn_fatory)

#make_nums <- function(x){
#  eval(parse(text = paste0(x, "<<-", str_extract(x,"\\d+"))))
#}
#
#vars|>unlist()|>str_extract("`\\d+`")|>unique()|>na.omit()|>walk(make_nums)

#Part 1 #16076
a

