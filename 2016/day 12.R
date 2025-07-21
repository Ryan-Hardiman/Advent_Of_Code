source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(12,2016)

get_instructions <- function(x){
  x%>%str_match_all("[-]*\\w+") 
} 

get_val <- function(x,env){
  if (grepl("\\d", x)) { 
    val <- parse_number(x) 
  } else{
    val <- get(x, envir = env) # Get value of variable named x 
  }
  val
}


cpy <- function(x, y,idx,env) {
  val <- get_val(x, env)
  assign("idx", idx+1, envir = env) 
  assign(y, val, envir = env) # Assign it to variable named y 
} 

inc <- function(x,idx,env){ 
  val <- get(x, env) 
  assign("idx",idx+1,envir = env) 
  assign(x, val+1,envir = env) 
} 

dec <- function(x,idx,env){ 
  val <- get(x, env) 
  assign("idx", idx+1, envir = env) 
  assign(x, val-1, envir = env) 
} 

jnz <- function(x,y,idx,env){ 
  chk <- get_val(x,env)
  val <- get_val(y,env)
  if(chk == 0){ 
    assign("idx", idx+1, envir=env) 
  } else{
    assign("idx",idx+val,envir=env) 
  } 
} 

monorail <- function(x,bump_e=F){ 
  #We will work in environments
  env <- new.env()
  #initialise params 
  env$a<-0 
  env$b<-0 
  env$e<-0 +1*bump_e#c = e and e = c - got it?!
  env$d<-0 
  env$idx<-1 
  
  #Get instructions 
  instrs <- get_instructions(x) %>%lapply(str_replace,"^c$","e")
  valid <- seq_along(instrs)
  while(env$idx %in% valid ){ 
    cat(sprintf( #Pure debug overkill
      "idx: %-4d | instr: %-15s | a: %-5d | b: %-5d | e: %-5d | d: %-5d\n",
      env$idx,
      paste(instrs[[env$idx]], collapse = " "),
      env$a,
      env$b,
      env$e,
      env$d
    ))
    instr <-instrs[[env$idx]]
    fn <- match.fun(instr[[1]])
    args <- c(instr[-1], list(env$idx, env)) #Throw in the environment too!
    do.call(fn,args) 
  } 
  
  return(env$a) 
}

part_1 <- function(x){
  x%>%monorail
}

part_2 <-function(x){
x%>%monorail(T) #There is definitely a "trick" using some polynomial math.
}

#Answers:
part_1(data) # 318007
part_2(data) # 
