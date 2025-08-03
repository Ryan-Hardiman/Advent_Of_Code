source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
library(crayon) #Fun console print
data<- get_aoc(23,2016)

#Mostly from day 12:
get_instructions <- function(x){
  x%>%str_match_all("[-]*\\w+") 
} 

get_val <- function(x,env){
  x <- as.character(x)
  if (grepl("\\d", x)) { 
    val <- parse_number(x) 
  } else{
    val <- get(x, envir = env) # Get value of variable named x 
  }
  val
}


cpy <- function(x, y,idx,env) {
  if (!grepl("\\d", y)) {
  val <- get_val(x, env)
  assign(y, val, envir = env) # Assign it to variable named y 
  }
  assign("idx", idx+1, envir = env)
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

tgl <- function(x,idx,env){
  tmp_instrs <- get_val("instrs",env)
  offset <- get_val(x,env) 
  target <- idx + offset
  
  #If legal, make a switch.
  if (target %in% seq_along(tmp_instrs)) {
    #Lets now walk through the rules, first: Inc --> Dec
    if (tmp_instrs[[target]][[1]] == "inc") {
      tmp_instrs[[target]][[1]] <- "dec"
    } else if (tmp_instrs[[target]][[1]] %in% c("dec", "tgl")) {
      #For singular value exprs: Expr --> Inc
      tmp_instrs[[target]][[1]] <- "inc"
    } else if (tmp_instrs[[target]][[1]] == "jnz") {
      #Jnz --> Cpy
      tmp_instrs[[target]][[1]] <- "cpy"
    } else{
      #Cpy --> Jnz
      tmp_instrs[[target]][[1]] <- "jnz"
    }
    
    #Replace the instruction.
    assign("instrs", tmp_instrs, envir=env)  
    
    
    # Invalidate any fast-paths that touch the modified instruction
    env$fastpath <- Filter(function(fp) {
      !(target >= fp$start && target <= fp$end)
    }, env$fastpath)
    
    # Re-scan and re-add
    env$fastpath <- c(env$fastpath, detect_optimisations(env$instrs,env))
    
  }
  
  #We've consumed a step, so add 1
  assign("idx", idx+1, envir=env) 
}

monorail <- function(x,eggs){ 
  #We will work in environments
  env <- new.env()
  #initialise params 
  env$a<-eggs
  env$b<-0 
  env$e<-0
  env$d<-0 
  env$idx<-1 
  env$fastpath <- vector("list") #Used to short-cut executions for known operations (see "detect optimisations_below") 
  

  #Get instructions 
  instrs <- get_instructions(x) %>%lapply(str_replace,"^c$","e")
  env$instrs <- instrs
  
  #Initialise optimisations 
  env$fastpath <- detect_optimisations(env$instrs, env)
  
  valid <- seq_along(instrs)
  while(env$idx %in% valid ){ 
    # Check for fastpath optimisation at current idx
    matched <- NULL
    for (opt in env$fastpath) {
      if (env$idx == opt$start) {
        matched <- opt
        break
      }
    }
    
    if (!is.null(matched)) {#Pure debug overkill but this is fun and ACTUALLY USEFUL!
      if(length(matched)>4) print(matched)
      cat(sprintf(
        "%s idx: %-4d | instr: %-24s | a: %-10d | b: %-5d | e: %-6d | d: %-10d | %s\n",
        green("OPTIMISED:"),
        env$idx,
        sprintf("fastpath (%3d → %3d)", matched$start, matched$end),
        env$a,
        env$b,
        env$e,
        env$d,
        matched$name
      ))
      matched$fn(env)        # Run the optimized fast-path code
      next                   # Skip normal instruction handling
    }
    
    
    
    cat(sprintf( #Pure debug overkill but this is fun and ACTUALLY USEFUL!
      "%s idx: %-4d | instr: %-22s | a: %-10d | b: %-5d | e: %-6d | d: %-10d\n",
      "EXECUTING:",
      env$idx,
      paste(env$instrs[[env$idx]], collapse = " "),
      env$a,
      env$b,
      env$e,
      env$d
    ))
    instr <-env$instrs[[env$idx]]
    fn <- match.fun(instr[[1]])
    args <- c(instr[-1], list(env$idx, env)) #Throw in the environment too!
    
    
    #The below was used to investigate the "optimisations"
    #cat(env$idx, paste(instrs[[env$idx]], collapse = " "), "\n")
    
    do.call(fn,args) 
  } 
  
  return(env$a) 
}

#Now for the fun part, detecting and using patterns to shortcut the number
#of executions required.
detect_optimisations <- function(instrs,env){
  optimisations <- vector("list")
  matched_ranges <- vector("list")
  
  # Helper to check for overlap with already matched ranges
  overlaps <- function(start, end) {
    any(sapply(matched_ranges, function(r) {
      max(start, r$start) <= min(end, r$end)
    }))
  }
  
  # --- Pattern 1: Multiplication loop ---
  #
  # Matches a loop of the form:
  #   cpy b e      ; copy value of b into e
  #   inc a        ; increment a
  #   dec e        ; decrement e
  #   jnz e -2     ; loop back to inc a while e != 0
  #   dec d        ; decrement d
  #   jnz d -5     ; outer loop: repeat the above loop d times
  #
  #This can be shortened to: 
  # a = a + b*d ; e  = 0 ; d = 0
  for (i in seq_len(length(instrs)-5)){
    start <- i
    end <- i + 5
    if (overlaps(start, end)) next
    
    poss_patrn <- instrs[start:end]
    if(
      all(
        poss_patrn%>%lapply(pluck,1)%>%unlist() == 
        c("cpy","inc","dec","jnz","dec","jnz")) &&
      poss_patrn[[4]][3] == "-2" &&
      poss_patrn[[6]][3] == "-5" &&
      poss_patrn[[4]][2] == poss_patrn[[3]][2] &&
      poss_patrn[[6]][2] == poss_patrn[[5]][2]
    ){
     tmp_b <- poss_patrn[[1]][2]
     tmp_a <- poss_patrn[[2]][2]
     tmp_d <- poss_patrn[[5]][2]
     
     optimisations[[length(optimisations)+1]] <- list(
       name = "Multiplication", #First 3 for cat statement
       start = start,
       end = end,
       fn = function(env){
         assign("a", get_val(tmp_a, env) + get_val(tmp_b, env) * get_val(tmp_d, env), envir = env)
         assign("e",0,envir = env)
         assign("d",0,envir= env)
         assign("idx", env$idx+6,envir = env)
         }
     )
     matched_ranges[[length(matched_ranges)+1]] <- list(start = start, end = end)
    }
  }
  
  # --- Pattern 2: Addition loop ---
  #
  # Matches a loop of the form:
  #   inc a        ; increment a
  #   dec e        ; decrement e
  #   jnz e -2     ; loop back to inc a while e != 0
  #This can be shortened to:
  # a = a + e ; e  = 0
  for (i in seq_len(length(instrs)-2)){
    start <- i
    end <- i + 2
    if (overlaps(start, end)) next

    poss_patrn <- instrs[start:end]
    if(
      all(
        poss_patrn%>%lapply(pluck,1)%>%unlist() ==
        c("inc","dec","jnz")) &&
      poss_patrn[[3]][3] == "-2" &&
      poss_patrn[[2]][2] == poss_patrn[[3]][2]
    ){
      #print(poss_patrn)
      tmp_add <- poss_patrn[[1]][2]
      tmp_dec <- poss_patrn[[2]][2]

      optimisations[[length(optimisations)+1]] <- list(
        name = "Addition 1", #First 3 for cat statement
        start = start,
        end = end,
        fn = function(env){
          assign(tmp_add, get_val(tmp_add, env) + get_val(tmp_dec, env), envir = env)
          assign(tmp_dec,0,envir = env)
          assign("idx", env$idx+3,envir = env)
        }
      )
      matched_ranges[[length(matched_ranges)+1]] <- list(start = start, end = end)
    } 
  }

  # --- Pattern 3: Addition loop 2 ---
  #
  # Matches a loop of the form:
  #   dec d        ; decrement d
  #   inc e        ; increment e
  #   jnz d -2     ; loop back to inc d while d != 0
  #This can be shortened to:
  # # e = e + d ; d  = 0
  for (i in seq_len(length(instrs)-2)){
    start <- i
    end <- i + 2
    if (overlaps(start, end)) next

    poss_patrn <- instrs[start:end]
    if(
      all(
        poss_patrn%>%lapply(pluck,1)%>%unlist() ==
        c("dec","inc","jnz")) &&
      poss_patrn[[3]][3] == "-2" &&
      poss_patrn[[1]][2] == poss_patrn[[3]][2] &&
      poss_patrn[[2]][2] != poss_patrn[[1]][2]
    ){
      tmp_dec <- poss_patrn[[1]][2]
      tmp_add <- poss_patrn[[2]][2]

      optimisations[[length(optimisations)+1]] <- list(
        name = "Addition 2",
        start = start,
        end = end,
        fn = function(env){
          assign(tmp_add, get_val(tmp_add, env) + get_val(tmp_dec, env), envir = env)
          assign(tmp_dec,0,envir = env)
          assign("idx", env$idx+3,envir = env)
        }
      )
      matched_ranges[[length(matched_ranges)+1]] <- list(start = start, end = end)
    }
  }

#
  #I can see that the next - full optimisation (for my input) is to handle 
  # tlg e --> cpy -16 e --> jnz 1 e --> cpy a d --> cpy 0 a --> multiplication. 
  
  #This just takes a and multiplies it by b.
  #Sets d to 0 
  #Sets e to 0
  
  #There's some inner loop doing 12! and some additional part to make each 
  # input unique. I feel that's a little too meta so I'll stop here. 
  
  
  
  
      
  return(optimisations)
}



part_1 <-function(x){
x%>%monorail(7)
}


part_2 <-function(x){
x%>%monorail(12)
}

#Answers:
part_1(data) # 12654

part_2(data) #479009214
