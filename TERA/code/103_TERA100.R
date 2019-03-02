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

# 역세권
data[,역세권:=ifelse(grepl("역 ",x1),"1","0")]

# 주택 
tmp = c("단독","공동","생활","다가구","연립","상가주택","전원")
data[,주택:=ifelse(grepl("주택",x1),"1","0")]
for(i in 1:length(tmp)){
  data[주택=="1",주택:=ifelse(grepl(tmp[i],x1),tmp[i],주택)]  
}
data[,주택:=ifelse(주택=="1","기타",주택)]
data[주택!="0",주택:=paste0(주택,"주택")]
data[주택!="0",주택:=ifelse(주택=="상가주택주택","상가주택",주택)]
table(data$주택)

# 건물
tmp =  c(
  "아파트","타운하우스","주택","오피스텔","빌딩","상가","빌라",
  "건축자금","원룸","다세대","펜션","다가구","토지","렌탈하우스",
  "생활시설","담보","숙박","대지조성","태양","테라스하우스"
)
data[,건물:=주택]
for(i in 1:length(tmp)){
  data[주택=="0",건물:=ifelse(grepl(tmp[i],x1),tmp[i],건물)]
}
data[,건물:=ifelse(건물=="다가구","다가구주택",건물)]
data[,건물:=ifelse(건물=="다세대","다세대주택",건물)]
data[,건물:=ifelse(건물=="0","기타",건물)]
data[,주택:=ifelse(grepl("주택",건물),"1","0")]
table(data$주택)
table(data$건물)

# 투자유형
tmp = c("재건축", "재개발", "신축", "리파이낸싱", "담보", "건축자금", "근질권")
data[,투자유형:=NA]
for(i in 1:length(tmp)){
  data[,투자유형:=ifelse(grepl(tmp[i],x1),tmp[i],투자유형)]
}
table(data$투자유형)

# 도시유형
tmp = c("신도시", "개발지구", "마을")
data[,도시유형:=NA]
for(i in 1:length(tmp)){
  data[,도시유형:=ifelse(grepl(tmp[i],x1),tmp[i],도시유형)]
}
table(data$도시유형)

# 지역2
data[,지역2:=gsub("_.*","",지역2)] 
tmp = fread(file.path(PATH_INPUT,"TERA_DIV2_DICT.csv"))$word
for(i in 1:length(tmp)){
  data[,지역2:=ifelse(grepl(tmp[i],지역2),"",지역2)]
}
data[,지역2:=ifelse(nchar(지역2)<=4,지역2,"")]

# 지역
tmp = fread(file.path(PATH_INPUT,"TERA_DIV1_DICT.csv"))$word
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
data[,':='(x1=NULL,x2=NULL)]
head(data)

# save
write.csv(data,file.path(PATH_OUTPUT,"data.csv"),row.names = F)

