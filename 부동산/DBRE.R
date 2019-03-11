# title : DBRE.R
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
library(leaflet.extras)
library(RColorBrewer)

# RCOLORS
RCOLORS = brewer.pal(n = 8, name = 'Pastel2')

# set gmap api
register_google(key="AIzaSyCXh01QH5qS9MjT1thxp63fbVBtNlsebgc")

# check lat and long
seoul_lonlat = geocode('서울', source = 'google')

# path 
PATH_INPUT = "c:/Users/user/Documents/부동산/input"

# read data
data = fread(file.path(PATH_INPUT,"DT_SEOUL.csv"))
head(data)

# ----
# year month
# ----

data[,year:=substr(계약년월,1,4)]
data[,month:=substr(계약년월,5,6)]

ply = data %>% count(year) %>% plot_ly(x = ~year, y = ~n, type='bar') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

ply = data %>% count(month) %>% plot_ly(x = ~month, y = ~n, type='bar') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

ply = data %>% count(year,month) %>% plot_ly(x = ~month, y = ~n, color= ~as.factor(year), type='scatter',mode = 'lines+markers') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

# ----
# 시군구 
# ----

# head

tmp = data %>% count(시군구) %>% arrange(desc(n)) %>% head(50)
ply = tmp %>% plot_ly(x = ~시군구, y = ~n, type="bar") %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

tmp = data %>% count(시군구,year) %>% filter(시군구 %in% tmp$시군구)
ply = tmp %>% plot_ly(x = ~시군구, y = ~n, color= ~as.factor(year), type='scatter',mode = 'lines+markers') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

# tail

tmp = data %>% count(시군구) %>% arrange(desc(n)) %>% tail(50)
ply = tmp %>% plot_ly(x = ~시군구, y = ~n, type="bar") %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

tmp = data %>% count(시군구,year) %>% filter(시군구 %in% tmp$시군구)
ply = tmp %>% plot_ly(x = ~시군구, y = ~n, color= ~as.factor(year), type='scatter',mode = 'lines+markers') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

# ----
# 지역
# ----

data[,지역_구:=sapply(시군구,function(x) paste0(unlist(strsplit(x,split=" "))[1:2],collapse = " "))]
data[,지역_동:=sapply(시군구,function(x) unlist(strsplit(x,split=" "))[3])]

tmp = data %>% count(지역_구) %>% arrange(desc(n)) 
ply = tmp %>% plot_ly(x = ~지역_구, y = ~n, type="bar") %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

tmp = data %>% count(지역_구,year) %>% filter(지역_구 %in% tmp$지역_구)
ply = tmp %>% plot_ly(x = ~지역_구, y = ~n, color= ~as.factor(year), type='scatter',mode = 'lines+markers') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

# ----
# 전용면적
# ----

data[,전용면적:=signif(`전용면적(㎡)`,2)]

ply = data %>% count(전용면적) %>% plot_ly(x = ~전용면적, y = ~n, type="bar") %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

ply = data %>% count(전용면적,year) %>% plot_ly(x = ~전용면적, y = ~n, color= ~as.factor(year), type='scatter',mode = 'lines+markers') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

tmp = data %>% count(전용면적,year,month) %>% filter(전용면적 %in% 60)
ply = tmp %>% plot_ly(x = ~month, y = ~n, color= ~as.factor(paste0(전용면적,"㎡_",year)), type='scatter',mode = 'lines+markers') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

tmp = data %>% count(전용면적,year,month) %>% filter(전용면적 %in% 85)
ply = tmp %>% plot_ly(x = ~month, y = ~n, color= ~as.factor(paste0(전용면적,"㎡_",year)), type='scatter',mode = 'lines+markers') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

tmp = data %>% count(전용면적,year,month) %>% filter(전용면적 %in% 110)
ply = tmp %>% plot_ly(x = ~month, y = ~n, color= ~as.factor(paste0(전용면적,"㎡_",year)), type='scatter',mode = 'lines+markers') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

# ----
# 기타 
# ----

# 층
data[,층:=as.numeric(층)]
ply = data %>% count(층) %>% plot_ly(x = ~층, y = ~n, type="bar") %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

# 지역_동 평균 건축년도 (head)
tmp = unique(data[,list(지역_동,단지명,건축년도)])
tmp = tmp[,list(건물_평균나이=round(2019-mean(건축년도))),by=list(지역_동)]
tmp = head(tmp[order(건물_평균나이,decreasing = F),],50)
ply = tmp %>% plot_ly(x = ~지역_동, y = ~건물_평균나이, type="bar") %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

# 지역_동 평균 건축년도 (tail)
tmp = unique(data[,list(지역_동,단지명,건축년도)])
tmp = tmp[,list(건물_평균나이=round(2019-mean(건축년도))),by=list(지역_동)]
tmp = tail(tmp[order(건물_평균나이,decreasing = F),],50)
ply = tmp %>% plot_ly(x = ~지역_동, y = ~건물_평균나이, type="bar") %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

# 지역_구 평균 건축년도 
tmp = unique(data[,list(지역_구,단지명,건축년도)])
tmp = tmp[,list(건물_평균나이=round(2019-mean(건축년도))),by=list(지역_구)]
tmp = tmp[order(건물_평균나이,decreasing = F),]
ply = tmp %>% plot_ly(x = ~지역_구, y = ~건물_평균나이, type="bar") %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

# map
div = unique(data[,list(시군구,lat,lon)])
leaflet() %>% 
  setView(lng=seoul_lonlat[1],lat=seoul_lonlat[2], zoom=10) %>%
  addProviderTiles("Esri.WorldImagery", group="인공위성") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="일반지도") %>%
  addMarkers(data=div, lng=~lon, lat=~lat, popup = ~시군구, group = 'marker', label = ~시군구, icon = ~ icons(
    iconUrl = 'https://static.thenounproject.com/png/481137-200.png')) %>%
  addSearchFeatures('marker') %>%
  addLayersControl(baseGroups = c("인공위성","일반지도"), options = layersControlOptions(collapsed = FALSE))

# ----
# 거래금액
# ----

data[,거래금액_만원:=as.numeric(gsub(",","",`거래금액(만원)`))]
data[,거래금액_만원:=signif(거래금액_만원,3)]
ply = data %>% count(거래금액_만원) %>% plot_ly(x = ~거래금액_만원, y = ~n, mode="markers", type="scatter") %>% layout(xaxis=list(fixedrange=TRUE,title="거래금액(200k는 20억)")) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

# [특정] 지역, 전용면적

tmp = data.table(table(data$전용면적))
head(tmp[order(N,decreasing = T),],3)

# 하왕십리동

tmp = data[지역_동 %in% c("하왕십리동") & 전용면적 %in% c(60,85,110),]
ply = tmp %>% count(단지명,전용면적) %>% plot_ly(x = ~전용면적, y = ~n, color= ~as.factor(단지명),type="bar") %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)

tmp = data[지역_동 %in% c("하왕십리동") & 전용면적 %in% c(60,85,110),]
ply = tmp %>% plot_ly(x = ~as.Date(paste(year,month,"01",sep="-")), y = ~거래금액_만원, color= ~as.factor(paste0(전용면적,"㎡_",단지명)),type='scatter',mode = 'markers') %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
plotly::config(ply, displayModeBar = F)



