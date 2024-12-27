source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(17,2024)

parse_data<-function(x){
x<-x|>str_extract_all("\\d+")|>lapply(as.numeric)
assign("A",x[[1]],envir = globalenv())
assign("B",x[[2]],envir = globalenv())
assign("SEE",x[[3]],envir = globalenv())
assign("Prog",x[[5]],envir = globalenv())
}

#Conversions
to_binary <- function(myx){rev(1*as.logical(intToBits(myx%%8)[1:3]))}
to_int <- function(myx){(myx*2^(3:1-1))|>sum()}

combo <- function(x){
if(x %in%c(0:3)){return(x)}
if(x ==4){return(A)}
if(x ==5){return(B)}
if(x ==6){return(SEE)}
if(x ==7){stop("Error - 7 should not occur.")}
}


move<-function(Prog,ptr){ 
  
  if(ptr+1 > length(Prog)-1){return(output)}
  instruction_ptr <- Prog[ptr+1]
  operand_ptr <- Prog[ptr +2]

  if(instruction_ptr == 0){adv(combo(operand_ptr))}
  if(instruction_ptr == 1){bxl(operand_ptr)}
  if(instruction_ptr == 2){bst(combo(operand_ptr))}
  if(instruction_ptr == 4){bxc(Inf)}
  if(instruction_ptr == 5){out(combo(operand_ptr))}
  if(instruction_ptr == 6){bdv(combo(operand_ptr))}
  if(instruction_ptr == 7){cdv(combo(operand_ptr))}
  if(instruction_ptr == 3){
    if(is.null(A)){ptr <- ptr +2; return(move(Prog,ptr))}
    if(A!=0){ptr <- operand_ptr-2}
  }
  ptr<-ptr + 2
  move(Prog,ptr)
}


adv <- function(x){
  assign("A",floor(A/(2^x)), env = globalenv())
}

bxl <- function(x){
assign("B",((xor(intToBits(x),intToBits(B))|>as.logical()*1)*2^(0:31))|>sum(),envir =globalenv())
}

bst <- function(x){
  assign("B",x%%8, env = globalenv())
}


bxc <- function(x){
  assign("B",((xor(intToBits(SEE),intToBits(B))|>as.logical()*1)*2^(0:31))|>sum(),envir =globalenv())
}

out <- function(x){#Call oupout 
  assign("output",c(output,x%%8),env= globalenv())
}

bdv <- function(x){
  assign("B",floor(A/(2^x)), env = globalenv())
}

cdv <- function(x){
  assign("SEE",floor(A/(2^x)), env = globalenv())
}

part_1 <-function(x){
assign("output", c(), envir = globalenv()) #Initialising
x <- parse_data(x)
move(Prog,0)|>paste0(collapse=",")|>noquote()
}


#Turns out there's base R bit-wise xor... who knew!

#Re-worked solution which only follows my Input needs. 
#Uses base binary functions
my_fn <- function(a){
  bee <- bitwXor(7L, (a %% 8L))
  cee <- a / 2^bee
  bitwXor(7L, bitwXor(bee,cee %% 8L))
}


part_2 <-function(x){
  curr <- 0
  for (i in rev(seq_along(Prog))) {
    curr2 <- numeric()
    for(.curr in curr) {
      out <-map(.curr * 8+ 0:7,my_fn)
      curr2 <- c(curr2, which(out == Prog[i]) - 1 + .curr * 8)
    }
    curr <- curr2
  }
  min(curr)
}


#Answers:
part_1(data) # 2,1,0,1,7,2,5,0,3
part_2(data) #267265166222235




