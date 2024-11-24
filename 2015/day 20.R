data <- clipr::read_clip()|>as.numeric()

allFactors <- function(n) {
  v <- trunc(c(1:sqrt(n)))
  f <- v[n %% v == 0]
  unique(sort(c(f, n / f)))
}

count_presents <- function(x,mult){
  (mult*allFactors(x))|>sum()
}

#Try bruteforce?
i<-1
repeat{
i<-i+1
if(count_presents(i,10) >=data) break
    
}

#part_1  
i

#Minor tweak , maybe bruteforce again? 
allFactors <- function(n) {
  v <- trunc(c(1:sqrt(n)))
  f <- v[n %% v == 0]
  a <-unique(sort(c(f, n / f)))
  a[a*50>=n]
}

count_presents <- function(x,mult,skip){
  (mult*allFactors(x,skip))|>sum()
}

#part_2 
i<-1
repeat{
  i<-i+1
  if(count_presents(i,11) >=data) break
}

i
