source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(22,2016)

parse_data <- function(x){
  x%>%tail(-2)%>%
    str_split(" +")%>%
    reduce(rbind)%>%
    as_tibble()%>%
    set_names(c("xy","s","u","a","p"))%>%
    separate(xy,c("to_drop","x","y"),sep ="-")%>%
    select(-to_drop)%>% #Avoid warning msg.
    mutate(across(everything(),parse_number),
           xy = map2_vec(x,y,~complex(1,.x,.y)))
}


part_1 <-function(x){
df <- parse_data(x)

pair_vec <- expand.grid(
  xy1=df$xy%>%unique(),
  xy2=df$xy%>%unique()
  )

pair_vec%>%
  left_join(df,by = c("xy1"="xy"))%>%
  left_join(df,by = c("xy2"="xy"))%>%
  filter(u.x>0, xy1!=xy2,u.x<=a.y)%>%nrow()


}


part_2 <-function(x){
x
}

#Answers:
part_1(data) #946
part_2(data) #