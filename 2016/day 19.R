source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(19,2016)%>%parse_number()

#For part 1 we keep the odd items in the vector, rotating if the vector is odd length.
#This takes at most n steps where x <= 2^n. Much faster than popping 1 at a time. 
take_next <-function(vec){
  if(length(vec)%%2==0){
  Tailcall(take_next,vec[which(seq(length(vec))%%2==1)])
  }else if(length(vec)!=1){
    new_head <- tail(vec,1)
    vec <- head(vec,-1)
    Tailcall(take_next,c(new_head,vec[which(seq(length(vec))%%2==1)]))
  }else return(vec)
}


take_opposite <- function(vec,idx){
if(length(vec)==1) return(vec)
if(idx %%10000 == 0) print(idx)
new <- vec[-ceiling(idx/2)]
Tailcall(take_opposite, c(tail(new,-1),head(new,1)),idx-1)
}




part_1 <-function(x){ 
take_next(1:x)
}



part_2 <-function(x){
magic <- 3^floor(log(x,3))

if(x == magic) return(x)
if(x <= 2*magic) return(x-magic)
2*x-3*magic

}

#Answers:
part_1(data) # 2097151 too high
part_2(data) # 1423634 - I dont like this. 


#Investigation: 

#Look for pattern. Since this doesn't seem plausible using some math. .
# Generate results for sizes 1 to 1000
results <- map(1:1000,~c(.x,take_opposite(1:.x,.x+1)))%>%reduce(rbind)%>%as_tibble()
colnames(results) <- c("x","y")
 
# Plot
ggplot(results, aes(x = x, y = y)) +
  geom_line() +
  labs(title = "Winning Thief over participants 1:1000",
       x = "Participants",
       y = "Final_Thief") +
  theme_minimal()

#We see that there are Sharp returns to thief 1 at every power of 3 +1.
#I.e at 4,10,28

#Verift
results%>%filter(y==1,x>3)%>%mutate(check = 3^row_number(x)+1==x)

#We then see that it is linear in two parts, split by a gradient change.
#Visually inspecting, we see that the step change is intiially in increments of 1. 
#This then rises to 2 at some point, which appears to be ~ halfway through the 
#duration between powers of 3. 

results%>%mutate(pwr_3 = ceiling(log(row_number(x),3)))%>%
  group_by(pwr_3)%>%mutate(cnt=row_number(),
                           chk=cnt<=max(cnt)/2)

#Yes that appears to be it - Or I've just magically found something that fits the first 1000 only.. 

