library(openssl)

ID <- md5(paste0(clipr::read_clip(),1:10000000))

part1<-which(!is.na(str_extract(ID,"^[0]{5}")))|>head(1)
part1

part2<-which(!is.na(str_extract(ID,"^[0]{6}")))|>head(1)
part2
