# title : 100_DBRE.R
# author : hjy

# library
library(data.table)
library(readxl)

# path 
PATH_INPUT = "c:/Users/user/Documents/부동산/input"

# read data
files = list.files(file.path(PATH_INPUT,"경기도"))
data_list = list()
for(i in 1:length(files)){
  data_list[[i]] = readxl::read_excel(file.path(PATH_INPUT,"경기도",files[i]),skip=16)  
}
data = rbindlist(data_list)
head(data)

fwrite(data,"./sample1.csv")


# read data
files = list.files(file.path(PATH_INPUT,"서울시"))
data_list = list()
for(i in 1:length(files)){
  data_list[[i]] = readxl::read_excel(file.path(PATH_INPUT,"서울시",files[i]),skip=16)  
}
data = rbindlist(data_list)
head(data)

fwrite(data,"./sample2.csv")

