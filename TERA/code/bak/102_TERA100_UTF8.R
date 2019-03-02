# title : TERA 
# author : hjy

# library
library(data.table)

# path 
PATH_INPUT = "c:/Users/user/Documents/TERA/input"
PATH_OUTPUT = "c:/Users/user/Documents/TERA/output"

# read data
data_w_good = fread(file.path(PATH_INPUT,"TRDB_상환완료.csv"),fill=T)
head(data_w_good)

# 법인전용상품 제외 
data_w_good = data_w_good[-grep("#법인전용상품",x1),]
head(data_w_good)

# ----
# features 
# ----
data_w_good[,x1:=gsub("제 ","제",x1)]
data_w_good[,번호:=mapply("[[",strsplit(x1,split = " "),1)]
data_w_good[,번호:=gsub("[^0-9]","",번호)]
data_w_good[,번호:=as.numeric(번호)]
data.table(table(data_w_good$번호,useNA="always"))

data_w_good[,지역:=mapply("[[",strsplit(x1,split = " "),2)]
data.table(table(data_w_good$지역,useNA="always"))

data_w_good[,지역2:=mapply("[[",strsplit(x1,split = " "),3)]
data_w_good[,지역2:=mapply("[[",strsplit(지역2,split = "#"),1)]
data.table(table(data_w_good$지역2,useNA="always"))

data_w_good[,순위:=mapply("[[",strsplit(x1,split = "#"),2)]
data_w_good[,순위:=gsub("순위","",순위)]
data_w_good[,순위:=as.numeric(순위)]
data.table(table(data_w_good$순위,useNA="always"))

data_w_good[,tmp1:=mapply("[[",strsplit(x1,split = "#"),1)]
data_w_good[,tmp2:=gsub("^.* ","",tmp1)]
data_w_good[,차수:=gsub("[^0-9차]","",data_w_good$tmp2)]
data_w_good[,':='(tmp1=NULL,tmp2=NULL)]
data_w_good[,차수:=ifelse(차수=="","1차",차수)]
data_w_good[,차수:=gsub("차","",차수)]
data_w_good[,차수:=as.numeric(차수)]
data.table(table(data_w_good$차수,useNA="always"))

data_w_good[,리파이낸싱:=as.numeric(grepl("리파이낸싱",x1))]
data_w_good[,신축:=as.numeric(grepl("신축",x1))]
data_w_good[,다세대:=as.numeric(grepl("다세대",x1))]
data_w_good[,연립주택:=as.numeric(grepl("연립주택",x1))]
data_w_good[,타운하우스:=as.numeric(grepl("타운하우스",x1))]
data_w_good[,건축자금:=as.numeric(grepl("건축자금",x1))]
data_w_good[,담보:=as.numeric(grepl("담보",x1))]
data_w_good[,NPL:=as.numeric(grepl("NPL",x1))]
data_w_good[,근질권:=as.numeric(grepl("근질권",x1))]
data_w_good[,ABL:=as.numeric(grepl("ABL",x1))]
data_w_good[,x1:=NULL]
head(data_w_good)

data_w_good[,수익률:=mapply("[[",strsplit(x2,"[?]"),1)]
data_w_good[,수익률:=gsub("[^0-9.]","",수익률)]
data_w_good[,수익률:=as.numeric(수익률)]

data_w_good[,투자기간:=mapply("[[",strsplit(x2,"[?]"),2)]
data_w_good[,투자기간:=gsub("투자기간","",투자기간)]
data_w_good[,투자기간:=gsub("개월","",투자기간)]
data_w_good[,투자기간:=as.numeric(투자기간)]

data_w_good[,등급:=mapply("[[",strsplit(x2,"[?]"),3)]
data_w_good[,등급:=gsub("등급","",등급)]

data_w_good[,모집금액:=mapply("[[",strsplit(x2,"[?]"),4)]
data_w_good[,모집금액:=gsub("모집금액","",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="1억원","10000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="2억원","20000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="3억원","30000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="4억원","40000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="5억원","50000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="6억원","60000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="7억원","70000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="8억원","80000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="9억원","90000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="10억원","100000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="11억원","110000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="12억원","120000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="13억원","130000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="14억원","140000만원",모집금액)]
data_w_good[,모집금액:=ifelse(모집금액=="15억원","150000만원",모집금액)]
data_w_good[,모집금액:=gsub("억","",모집금액)]
data_w_good[,모집금액:=gsub(",","",모집금액)]
data_w_good[,모집금액:=gsub("만원","",모집금액)]
data_w_good[,모집금액:=gsub("[^0-9]","",모집금액)]
data_w_good[,모집금액:=as.numeric(모집금액)]

data_w_good[,투자인원:=mapply("[[",strsplit(x2,"[?]"),5)]
data_w_good[,투자인원:=gsub("투자인원","",투자인원)]
data_w_good[,투자인원:=gsub("[^0-9.]","",투자인원)]
data_w_good[,투자인원:=as.numeric(투자인원)]

data_w_good[,x2:=NULL]
data_w_good[,Y:="0"]
head(data_w_good)

# save 
fwrite(data_w_good,file.path(PATH_OUTPUT,"data_w_good.csv"))
write.csv(data_w_good,file.path(PATH_OUTPUT,"data_w_good.csv"),row.names = F)














