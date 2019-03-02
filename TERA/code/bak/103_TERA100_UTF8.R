# title : TERA 
# author : hjy

# library
library(data.table)
library(ggmap)
library(ggplot2)
library(grid)
library(gridExtra)
library(Kormaps)
library(tmap)
library(leaflet)

# path 
PATH_INPUT = "c:/Users/user/Documents/TERA/input"
PATH_OUTPUT = "c:/Users/user/Documents/TERA/output"

# read data
T101_TERA100 = fread(file.path(PATH_OUTPUT,"101_TERA100.csv"))
T102_TERA100 = fread(file.path(PATH_OUTPUT,"102_TERA100.csv"))
data = rbind(T101_TERA100,T102_TERA100,fill=T)

# missing values
data = data[지역!="주택담보부",]
data[is.na(data)] = 0

# preprocess
data[,지역:=gsub("신둔도예촌역","신둔도",지역)]
data[,지역2:=gsub("신도시","",지역2)]

# ----
# features 
# ----








# 지역2
data[,지역2:=sapply(strsplit(지역2,"_"),function(x) x[[1]])] 
tmp = fread(file.path(PATH_INPUT,"TERA_DIV2_DICT.csv"),encoding="UTF-8")
for(i in 1:length(tmp)){
  data[,지역2:=ifelse(grepl(tmp[i],지역2),"",지역2)]
}

# 지역
tmp = fread(file.path(PATH_INPUT,"TERA_DIV1_DICT.csv"))
for(i in 1:length(tmp)){
  data[,지역:=gsub(tmp[i],"",지역)]
}
data[,지역:=trimws(paste(지역,지역2,sep=" "))]

# set gmap api
register_google(key="AIzaSyCXh01QH5qS9MjT1thxp63fbVBtNlsebgc")

# check lat and long
seoul_lonlat = geocode('서울', source = 'google')

# extract lat and long
div_uniq = unique(data$지역)
div_list = list()
for(i in 1:length(div_uniq)){
  div_list[[i]] = geocode(div_uniq[i], source = 'google')
}
div = data.table(지역=div_uniq,rbindlist(div_list))
div[,지역:=as.character(지역)]

# merge
data = merge(data,div,by="지역",all.x=T)
head(data)

# save
write.csv(data,file.path(PATH_OUTPUT,"data.csv"),row.names = F)

