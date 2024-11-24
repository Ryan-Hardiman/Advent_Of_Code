data <- clipr::read_clip()
no1 <- str_extract(data,"\\d+")
no2 <- str_extract(data,"x\\d+")|>str_remove("x")
no3 <- str_extract(data,"\\d+$")

#part1
pmap(list(no1,no2,no3)|>lapply(as.numeric), function(a,b,c) 2*(b*c +a*c + a*b)+ min(a*b, b*c, a*c ) )|>unlist()|>sum()
#part2
pmap(list(no1,no2,no3)|>lapply(as.numeric), function(a,b,c) a*b*c+ min(2*(a+b), 2*(b+c), 2*(a+c) ) )|>unlist()|>sum()

