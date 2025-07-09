source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(10,2016)

# Assume that there are no loops so that we can just blindly run it.

#We only care about these 4 things. 
parse_data <- function(x){
  str_match_all(x,"bot|out|val|\\d+")
}

#Combine neighboring elems of vec e.g. c("a","b","c","d") --> c("ab", "cd")
pair_paste <- function(vec) {
  even_idx <- seq(2, length(vec), by = 2)
  odd_idx <- even_idx - 1
  paste0(vec[odd_idx], vec[even_idx])
}

#I will make a function factory, naming each function bot_<no>, taking as input 
# two numbers to compare and two goal numbers in which to return the bot no.
factory <- function(data, goal){
  
  my_env <- new.env() #Dipping toes..
  
  instructions <- parse_data(data)%>%lapply(pair_paste)
  
  #Make the things
  walk(
    instructions,
    \(x){
      if(grepl("val",x[1])){
        val <- as.integer(gsub("\\D","",x[1]))
        bot <- x[2]
        # Store initial assignments to run after bots exist
        if(!exists("init_ass",envir = my_env)){my_env$init_ass <- list()}
        my_env$init_ass[[length(my_env$init_ass)+1]] <- list(val= val, bot = bot)
      }else{
        #Make the bot, using all required data.
        create_bot(id = x[1], low_target =x[2],high_target = x[3],goal,my_env)
      }
    }
  )
  
  # After all bots are created, assign initial values
  walk(my_env$init_ass,    \(x) {
      add_value(x$val, x$bot, my_env)
    }
  )
  
  return(my_env)
}

create_bot <- function(id, low_target, high_target, goal,env){
<<<<<<< HEAD
=======
 # print(paste("Creating", id, "-> low to", low_target, ", high to", high_target)) # ← ADD
>>>>>>> 6a18e3980515d123c04eb1efc110763154fe8f5a
  bot_env <- new.env(parent = emptyenv())
  bot_env$id <- id
  bot_env$vals <- integer(0) #Make it blank first.
  bot_env$low_target <- low_target
  bot_env$high_target <- high_target
  
  #Now make the real bots:
  bot <- function(val){
    bot_env$vals <- c(bot_env$vals, val)
    
    #Upon recieving 2 chips, compare against goal and send on their way.
    if(length(bot_env$vals)==2){
      l_h <- sort(bot_env$vals)
      
      #Goal condition check
      if(all(l_h %in% goal)) env$goal_bot <- id
      
      add_value(l_h[1], bot_env$low_target, env)
      add_value(l_h[2], bot_env$high_target,env)
      
    }
  }
  
  delayedAssign(id, bot, assign.env = env)
  env[[paste0(id,"_env")]] <- bot_env
  
}

#Since we dont know what "value" we're sending, we will make a function to add any value. 
add_value <- function(val, target, env) {
  if (startsWith(target, "bot")) {
    if (!exists(target, envir = env)) {
      create_bot(target, NULL, NULL, env)
    }
    env[[target]](val)
    
  } else if (startsWith(target, "out")) {
    if (!exists(target, envir = env)) {
      env[[target]] <- integer(0)
    }
    env[[target]] <- c(env[[target]], val)
  }
}

part_1 <-function(x){
out <- factory(x,goal = c(17,61))
parse_number(out$goal_bot)
}


part_2 <-function(x){
  out <- factory(x, goal = NULL)  # goal not needed here
  outs<- c(out$out0[1], out$out1[1], out$out2[1])
  prod(outs)
}


#Answers:
part_1(data) # 141
part_2(data) # 1209
