source("get_aoc.R")
library(tidyverse)
data<-get_aoc(22,2015)|>str_extract("\\d+")|>as.numeric()
boss_start<-list("health"=data[[1]],"damage"=data[[2]])
my_start<- list("mana"=500,"health"=50)

magic <- list(
  "Magic_Missile" = c("mana"= 53,"damage"=4,"armour"=0,"heal"=0,"effect_dur"=0),
  "Drain" =         c("mana"= 73,"damage"=2,"armour"=0,"heal"=2,"effect_dur"=0),
  "Shield"=         c("mana"=113,"damage"=0,"armour"=7,"heal"=0,"effect_dur"=6),
  "Poison"=         c("mana"=173,"damage"=3,"armour"=0,"heal"=0,"effect_dur"=6),
  "Recharge"=       c("mana"=229,"damage"=0,"armour"=0,"heal"=0,"effect_dur"=5)
)

magic <- c("Recharge","Poison","Shield","Drain","Magic_Missile")

#Games will last somewhere in the realm of 20 moves I think...
#Where the permutations of games is 5^Moves.
#Which is too large to create, so lets try a new trick.. 
#Randomize each new move, memoise the move for that turn, mana, health etc.
#Simulate 10M+ games? and check result. 
#Do it again and see if the result is the same. 

action_choice <-function(turn=1,mana=my_start$mana,shield_remain=0,recharge_remain=0,poison_remain=0,max_mana=Inf,mana_spent=0,...){
  if(turn%%2==0){return(NA)}
  available<-magic

  if(shield_remain>1){available<-available[which(available!="Shield")]}
  if(poison_remain>1){available<-available[which(available!="Poison")]}
  if(recharge_remain>1){available<-available[which(available!="Recharge")]}
  avoid <- case_when(
    max_mana-mana_spent<229~list(c("Recharge")),
    max_mana-mana_spent<173~list(c("Recharge","Poison")),
    max_mana-mana_spent<113~list(c("Recharge","Poison","Shield")),
    max_mana-mana_spent<73~list(c("Recharge","Poison","Shield","Drain")),
    TRUE~list(c(""))
  )|>unlist()
  if(avoid != c("")){available<-available[!which(avoid %in%available)]}
  mana_in_turn <- mana + (recharge_remain>0)*101
  case_when(
    228<mana_in_turn~sample(available,1),
    172<mana_in_turn&mana_in_turn<229~sample(available[which(available %in%magic[-1])],1),
    112<mana_in_turn&mana_in_turn<173~sample(available[which(available %in%magic[-c(1:2)])],1),
    72<mana_in_turn&mana_in_turn<113~sample(available[which(available%in%magic[-c(1:3)])],1),
    52<mana_in_turn&mana_in_turn<73~"Magic_Missile",
    mana_in_turn&mana_in_turn<53~"NO MANA LEFT!"
  )|>action_to_numeric()
}

action_to_numeric <-function(action){which(magic==action)}

numeric_to_action <-function(number){magic[number]}

next_turn <- function(turn=1,
                      health=my_start$health,
                      mana=my_start$mana,
                      mana_spent=0,
                      boss_health=boss_start$health,
                      boss_damage=boss_start$damage,
                      shield_remain=0,
                      poison_remain=0,
                      recharge_remain=0,
                      action = NA,
                      hard_mode = FALSE){
  
  if(recharge_remain>0){
    mana<-mana+101
    recharge_remain<-recharge_remain-1
  }
  action<-numeric_to_action(action)
  
  if (turn %% 2 == 0) {
    # Boss Damage
    ifelse(
      poison_remain == 0,
      health <- health + 7 * (shield_remain > 0) - boss_damage,
      ifelse(boss_health - 3 <= 0, return(list("game_won" = mana_spent)), {
        boss_health <- boss_health - 3
        health <- health + 7 * (shield_remain > 0) - boss_damage
      })
    )
    shield_remain <- max(shield_remain - 1, 0)
    poison_remain <- max(poison_remain - 1, 0)
    if (health <= 0) {
      return(list("game_lost" = Inf))
    }  #Count deaths as consuming infinite Mana.
  } else{
    if(hard_mode){ifelse(health==1,return(list("game_lost"=Inf)),health<-health-1)}
    shield_remain <- max(shield_remain - 1, 0)
    if (poison_remain > 0) {
      poison_remain <- poison_remain - 1
      ifelse(boss_health - 3 <= 0,return(list("game_won" = mana_spent)), boss_health <- boss_health - 3)
    }
    if (action == "Magic_Missile") {
      mana_spent <- mana_spent + 53
      boss_health <- boss_health - 4
      mana <- mana - 53
    } else if (action == "Drain") {
      mana_spent <- mana_spent + 73
      mana <- mana - 73
      boss_health <- boss_health - 2
      health <- health + 2
    } else if (action == "Shield") {
      mana_spent <- mana_spent + 113
      mana <- mana - 113
      shield_remain <- 6
    } else if (action == "Poison") {
      mana_spent <- mana_spent + 173
      mana <- mana - 173
      poison_remain <- 6
    } else if (action == "Recharge") {
      mana_spent <- mana_spent + 229
      mana <- mana - 229
      recharge_remain <-5
    }
    if (boss_health <= 0) {
      return(list("game_won" = mana_spent))
    }
  }
  print(paste0("Turn: ",turn,"health:", health," mana:",mana," Boss Hp:",boss_health," Action:", unique(action), " Active:",if(shield_remain>0){paste0("Shield:",shield_remain,",")},if(poison_remain>0){paste0("Poison:",poison_remain,",")},if(recharge_remain>0){paste0("Recharge:",recharge_remain)}))
  #Debugging output is below:

  return(list("turn"=turn+1,
             "health"=health,
             "mana"=mana,
             "mana_spent"=mana_spent,
             "boss_health"=boss_health,
             "boss_damage"=boss_damage,
             "shield_remain"=shield_remain,
             "poison_remain"=poison_remain,
             "recharge_remain"=recharge_remain,
             "action"=action_to_numeric(action),
             "hard_mode"=hard_mode))
}

remember_turn <- memoise::memoise(next_turn)


run_simulations <- function(no_simulations,hard_mode=FALSE){
  i<- -1
  last_res <-list("init"=Inf)
  out<-list()
  max_mana<-Inf
  repeat{
    next_action <- do.call(action_choice,last_res)
    if(length(next_action)==0){last_res<-list("game_lost"=Inf)}
    if(any(c("init","game_lost","game_won")%in%names(last_res))){
      print(names(last_res))
      print(paste0("Starting game: ",i ))
      max_mana <- min(max_mana, last_res$game_won)
      out <- c(out,last_res)
      next_action <- action_choice(max_mana =max_mana)
      last_res <- remember_turn(turn=1,
                                health=my_start$health,
                                mana=my_start$mana,
                                mana_spent = 0,
                                boss_health = boss_start$health,
                                boss_damage = boss_start$damage,
                                shield_remain = 0,
                                poison_remain = 0,
                                recharge_remain = 0,
                                action = next_action,
                                hard_mode=hard_mode)
      i<-i+1
    }else{
        #We didnt have enough mana 

      last_res<-next_turn(last_res$turn,
                          last_res$health,
                          last_res$mana,
                          last_res$mana_spent,
                          last_res$boss_health,
                          last_res$boss_damage,
                          last_res$shield_remain,
                          last_res$poison_remain,
                          last_res$recharge_remain,
                          next_action,
                          hard_mode=hard_mode)}

    #print(i)
    if(i==no_simulations)break
    }

  return(unique(unlist(out)))
  
}


#Part 1 #900
part1 <- run_simulations(100000)
min(part1)

#refresh memoised function 
remember_turn <- memoise::memoise(next_turn)

#Part 2 #1216
part2 <- run_simulations(100000,TRUE)
min(part2)
