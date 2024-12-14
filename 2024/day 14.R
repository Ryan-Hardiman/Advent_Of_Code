source("get_aoc.R") #Pull input from website, prints input + stores it 
library(tidyverse) #Almost always required
data<- get_aoc(14,2024)
tst <- clipr::read_clip()

parse_data <- function(x){
  x<-x|>str_extract_all("[-]{0,1}\\d+")|>lapply(as.numeric)
  map(x, ~c(.x[1]+1i*.x[2], .x[3]+1i*.x[4]))
}

make_plot <- function(x){#Used for part 2, and making gif
  df <- tibble(complex = unlist(lapply(x,"[",1)))|>
    mutate(values1 = Re(complex),values2 = Im(complex),values3=row_number()%%12)
  
  p<-ggplot(df, aes(values1,values2))+ 
    geom_point(aes(color =values3))+  
    guides(color = FALSE)+ 
    labs(title = paste0("Robot Positions at T= ",nxt))+
    xlab("Dist from Left") + ylab("Dist from Top")+
    xlim(-1, grid_w+1) +ylim(-1,grid_w+1)
  
  ggsave(here::here("plots",paste0(nxt,".png")),p)
  rm(p)
}

#X is a vector of position and velocity. 
move <- function(x,nxt, grid_w, grid_h){
new_pos <- lapply(x, \(y) c(Re(y[1]+y[2])%%grid_w +
                              1i*(Im(y[1]+y[2])%%grid_h),
                            y[2]))

make_plot(x)
new_pos
}

repeated_move <- function(x,grid_w,grid_h,count = 100){
  reduce(1:count, move,.init = x, grid_w=grid_w, grid_h=grid_h)|>
    lapply("[",1)|>unlist()
}

score <- function(x,grid_w,grid_h){
  mid_col <- floor(grid_w/2)
  mid_row <- floor(grid_h/2)
  tl <- sum(Re(x)<mid_col & Im(x)<mid_row)
  tr <- sum(Re(x)>mid_col & Im(x)<mid_row)
  bl <- sum(Re(x)<mid_col & Im(x)>mid_row)
  br <- sum(Re(x)>mid_col & Im(x)>mid_row)

  prod(tl,tr,bl,br)
}


part_1 <-function(x,grid_w,grid_h){
x|>parse_data()|>repeated_move(grid_w,grid_h)|>score(grid_w,grid_h)
}

part_2 <-function(x,grid_w,grid_h,count){
  x|>parse_data()|>repeated_move(grid_w,grid_h,count)
}


#Answers:
part_1(data,101,103) #218965032

#Part 2 done visually from the plots - equally could have used minimum variance. 

part_2(data,101,103,10000) # 7037


#Now lets have some fun, grab the final 1000 frames and throw them in a gif. 
library(magick)
library(rgl)
filenames <- map(paste0(1:100 + 6938, ".png"), ~here::here("plots",.x))|>unlist()
m <- magick::image_read(filenames[1])
for (i in 2:100) 
  m <- c(m, magick::image_read(filenames[i]))
m <- magick::image_animate(m, fps = 20, loop = 1, dispose = "previous")
magick::image_write(m, "movie.gif")
