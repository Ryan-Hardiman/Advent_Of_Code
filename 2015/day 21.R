source("get_aoc.R")
library(tidyverse)
stats <- get_aoc(21,2015)|>str_extract("\\d+")|>as.numeric()
boss_start <- list("health" = stats[[1]],"damage" = stats[[2]],"armour" = stats[[3]], "id"=1)
my_start <- list("health"=100,"damage" = 0,"armour"=0,"id" =1)
store <- list(
  "Weapons" = list(
    "Dagger" =     c("cost"=8,"damage"=4,"armour"=0),
    "Shortsword" = c("cost"=10,"damage"=5,"armour"=0),
    "Warhammer" =  c("cost"=25,"damage"=6,"armour"=0),
    "Longsword" =  c("cost"=40,"damage"=7,"armour"=0),
    "Greataxe" =   c("cost"=74,"damage"=8,"armour"=0)
  ),
  "Armour" = list(
    "Leather" =    c("cost"=13,"damage"=0,"armour"=1),
    "Chainmail" =  c("cost"=31,"damage"=0,"armour"=2),
    "Splintmail" = c("cost"=53,"damage"=0,"armour"=3),
    "Banedmail" =  c("cost"=75,"damage"=0,"armour"=4),
    "Platemail" =  c("cost"=102,"damage"=0,"armour"=5),
    "NONE" =   c("cost"=0,"damage"=0,"armour"=0)
  ),
  "Ring1" = list(
    "Damage +1" = c("cost"=25,"damage"=1,"armour"=0),
    "Damage +2" = c("cost"=50,"damage"=2,"armour"=0),
    "Damage +3" = c("cost"=100,"damage"=3,"armour"=0),
    "Defense +1" = c("cost"=20,"damage"=0,"armour"=1),
    "Defense +2" = c("cost"=40,"damage"=0,"armour"=2),
    "Defense +3" = c("cost"=80,"damage"=0,"armour"=3),
    "NONE" =   c("cost"=0,"damage"=0,"armour"=0)
  ),
  "Ring2" = list(
    "Damage +1" = c("cost"=25,"damage"=1,"armour"=0),
    "Damage +2" = c("cost"=50,"damage"=2,"armour"=0),
    "Damage +3" = c("cost"=100,"damage"=3,"armour"=0),
    "Defense +1" = c("cost"=20,"damage"=0,"armour"=1),
    "Defense +2" = c("cost"=40,"damage"=0,"armour"=2),
    "Defense +3" = c("cost"=80,"damage"=0,"armour"=3),
    "NONE" =   c("cost"=0,"damage"=0,"armour"=0)
  )
  
)

edit_game <- function(x,var,repl){
  x[var]<-repl
  x
}

#This is every possible combination of Damage & Armour for the MINIMUM cost, adding ID to aid in filtering dead games. 
store_perms <-expand.grid(store)|>tibble()|>
  filter(!(names(Ring1)=="NONE"&names(Ring2)!="None")&(
    !(names(Ring1)==names(Ring2)) | (names(Ring1)=="NONE"&names(Ring2)=="NONE")))|>
  mutate(cost = pmap_dbl(list(Weapons,Armour,Ring1,Ring2),\(w,x,y,z){sum(c(w["cost"],x["cost"],y["cost"],z["cost"]))}))|>
  mutate(armour = pmap_dbl(list(Weapons,Armour,Ring1,Ring2),\(w,x,y,z){sum(c(w["armour"],x["armour"],y["armour"],z["armour"]))}))|>
  mutate(damage = pmap_dbl(list(Weapons,Armour,Ring1,Ring2),\(w,x,y,z){sum(c(w["damage"],x["damage"],y["damage"],z["damage"]))}))|>
  select(cost,armour,damage)|>unique()|>mutate(id = row_number(),
                                               me = list(my_start),
                                               boss = list(boss_start))|>mutate(
                                                 me = map2(id,me,~edit_game(.y,"id",.x)),
                                                 boss = map2(id,boss,~edit_game(.y,"id",.x)),
                                                 me = map2(damage,me,~edit_game(.y,"damage",.x)),
                                                 me = map2(armour,me,~edit_game(.y,"armour",.x)),
                                                 me = map2(cost,me,~edit_game(.y,"cost",.x)),
                                                 boss = map2(cost,boss,~edit_game(.y,"cost",.x))
                                               )



do_damage <- function(attacker,defender){
  defender["health"][[1]] <- defender["health"][[1]] -pmax(attacker["damage"][[1]]-defender["armour"][[1]],1)
  defender
}

next_turn <- function(boss,me,won_games =list(),index=1){
  #print(index)
  #print(won_games)
  ifelse(index%%2==1,boss <- map2(me,boss,do_damage),me<-map2(boss,me,do_damage))
  
  if(length(boss|>keep(~.$health<=0))>0){
    won_games <- list(won_games,unlist(lapply(boss|>keep(~.$health <= 0),pluck,"cost")))}
  
  boss <- boss|>keep(~.$health>0)
  me <- me|>keep(~.$health>0)
  if(length(me)==0){return(unlist(won_games))}
  ids <-intersect(unlist(lapply(boss,pluck,"id")),unlist(lapply(me,pluck,"id")))
  boss <- boss|>keep(~.$id %in%ids)
  me<-me|>keep(~.$id %in%ids)
next_turn(boss,me,won_games,index+1)
}

#Part1 - 111 
part1 <- next_turn(store_perms$boss,store_perms$me)
part1

rm(next_turn)#Re-define with middle if statement changed for me dying instead of boss.
next_turn <- function(boss,me,won_games =list(),index=1){
  ifelse(index%%2==1,boss <- map2(me,boss,do_damage),me<-map2(boss,me,do_damage))
  
  if(length(me|>keep(~.$health<=0))>0){
    won_games <- list(won_games,unlist(lapply(me|>keep(~.$health <= 0),pluck,"cost")))}
  
  boss <- boss|>keep(~.$health>0)
  me <- me|>keep(~.$health>0)
  if(length(me)==0){return(unlist(won_games))}
  ids <-intersect(unlist(lapply(boss,pluck,"id")),unlist(lapply(me,pluck,"id")))
  boss <- boss|>keep(~.$id %in%ids)
  me<-me|>keep(~.$id %in%ids)
  next_turn(boss,me,won_games,index+1)
}


part_2 <-next_turn(store_perms$boss,store_perms$me)
part_2|>max()
