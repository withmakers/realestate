# title : 102_DBRE.R
# author : hjy

# library
library(flexdashboard)
library(data.table)
library(knitr)
library(kableExtra)
library(plotly)
library(dplyr)
library(ggmap)
library(ggplot2)
library(grid)
library(gridExtra)
library(Kormaps)
library(tmap)
library(leaflet)
library(RColorBrewer)

# RCOLORS
RCOLORS = brewer.pal(n = 8, name = 'Pastel2')

# set gmap api
register_google(key="AIzaSyCXh01QH5qS9MjT1thxp63fbVBtNlsebgc")

# check lat and long
seoul_lonlat = geocode('서울', source = 'google')

# path 
PATH_INPUT = "c:/Users/user/Documents/부동산/input"
PATH_OUPUT = "c:/Users/user/Documents/부동산/output"

# read data
data = fread(file.path(PATH_INPUT,"부동산매매데이터_최근5년_서울시.csv"))
head(data)

# extract lat and long
div_uniq = unique(data$시군구)
div_list = list()
for(i in 1:length(div_uniq)){
  div_list[[i]] = geocode(div_uniq[i], source = 'google')
}
div = data.table(시군구=div_uniq,rbindlist(div_list))
div[,시군구:=as.character(시군구)]
head(div)

# merge
data = merge(data,div,by="시군구",all.x=T)
head(data)

# save
fwrite(data,file.path(PATH_INPUT,"DT_SEOUL.csv"))













