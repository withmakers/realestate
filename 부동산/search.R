library(leaflet)
library(leaflet.extras)

# check lat and long
seoul_lonlat = geocode('서울', source = 'google')

# extract lat and long
div_uniq = unique(data$시군구)
div_list = list()
for(i in 1:length(div_uniq)){
  div_list[[i]] = geocode(div_uniq[i], source = 'google')
}
div = data.table(시군구=div_uniq,rbindlist(div_list))
div[,시군구:=as.character(시군구)]

leaflet() %>% 
  setView(lng=seoul_lonlat[1],lat=seoul_lonlat[2], zoom=12) %>%
  addProviderTiles("Esri.WorldImagery", group="인공위성") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="일반지도") %>%
  addMarkers(data=div, lng=~lon, lat=~lat, popup = ~시군구, group = 'marker', label = ~시군구) %>%
  addSearchFeatures('marker') %>%
  addLayersControl(baseGroups = c("인공위성","일반지도"), options = layersControlOptions(collapsed = FALSE))






leaflet() %>% 
  setView(lng=seoul_lonlat[1],lat=seoul_lonlat[2],zoom=6) %>%
  
  # Add two tiles
  addProviderTiles("Esri.WorldImagery", group="인공위성") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="일반지도") %>%
  
  # Add 2 marker groups
  addCircleMarkers(data=data_blue, lng=~lon , lat=~lat, radius=~N, color="black",  fillColor="dodgerblue", stroke = TRUE, fillOpacity = 0.8, group="상환완료") %>%
  addCircleMarkers(data=data_red, lng=~lon , lat=~lat, radius=~N, color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="연체") %>%
  
  # Add the control widget
  addLayersControl(overlayGroups = c("연체","상환완료") , baseGroups = c("인공위성","일반지도"), options = layersControlOptions(collapsed = FALSE))
```  