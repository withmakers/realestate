# title : TERA100
# author : hjy

library(rmarkdown)
output_dir <- "./TERA"
render("./TERA/TERA100.Rmd", output_dir = output_dir)

# options
options(encoding="utf-8")
Sys.setlocale("LC_ALL","ko_KR.UTF-8")

# library
library(data.table)
library(h2o)
library(lfactors)
library(flexdashboard)
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

# set gmap api
register_google(key="AIzaSyCXh01QH5qS9MjT1thxp63fbVBtNlsebgc")

# path 
PATH_INPUT = "c:/Users/user/Documents/TERA/input"
PATH_OUTPUT = "c:/Users/user/Documents/TERA/output"

# read data
data = fread(file.path(PATH_OUTPUT,"data_UTF8.csv"),encoding="UTF-8")
head(data)
str(data)

# 지역
data[,지역2:=NULL]
unique(data$지역)

# data type
tmp = c('TARGET','지역','등급','역세권','주택','건물','투자유형','도시유형')
data[,(tmp):= lapply(.SD, as.factor), .SDcols = tmp]
head(data)

# select features
data = data[,list(
  TARGET,
  번호,
  차수,
  수익률,
  투자기간,
  등급,
  모집금액,
  역세권,
  주택,
  건물,
  투자유형,
  도시유형,
  lon,
  lat
)]

# colnames
colnames(data) = c(
  "TARGET",
  "number",
  "chasu",
  "earnRate",
  "period",
  "class",
  "gatherAmount",
  "stationDummy",
  "houseDummy",
  "buildType",
  "investType",
  "cityType",
  "lon",
  "lat"
)
str(data)

# FactorLevels
recordFactorLevels = function(data,featureName){
  FactorLevels = list()
  for(i in 1:length(featureName)){
    tmp = data.table(
      level = levels(data[,get(featureName[i])]),
      label = names(table(as.numeric(data[,get(featureName[i])])))
    )
    FactorLevels[[featureName[i]]] = tmp
  }
  return(FactorLevels)
}
FactorFeatures = c("TARGET","buildType","investType","cityType")
FactorLevels = recordFactorLevels(data,featureName = FactorFeatures)

# numeric factors
data[,(FactorFeatures):= lapply(.SD, function(x) as.factor(as.numeric(x))), .SDcols = FactorFeatures]
head(data)
str(data)

# h2o
h2o.init()

# hex
data_hex = as.h2o(data,encoding="UTF-8")
head(data_hex)

# ----
# train 
# ----

ml = h2o.automl(
  x = colnames(data_hex)[colnames(data_hex)!="TARGET"],
  y = 'TARGET',
  training_frame = data_hex,
  nfolds = 5,
  max_models = 100,
  max_runtime_secs = 60*60*24,
  exclude_algos = "DeepLearning" 
)
summary(ml@leader)
h2o.confusionMatrix(ml@leader)
h2o.varimp_plot(ml@leader)

# ----
# predict 
# ----

# get lat and lon
geocode('여주시', source = 'google')

# check 
FactorLevels
str(data_hex)

# test
test = data.table(
  number = c(2461),
  chasu = c(2),
  earnRate = c(14),
  period = c(4),
  class = c("C1"),
  gatherAmount = c(10000),
  stationDummy = c("0"),
  houseDummy = c("1"),
  buildType = c("7"),
  investType = c("4"),
  cityType = c(NA),
  lon = c(128),
  lat = c(37.3)
)
test_hex = as.h2o(test)
h2o.predict(ml@leader,newdata = test_hex)
# predict        p1          p2
# 1 0.9969781 0.003021862
















