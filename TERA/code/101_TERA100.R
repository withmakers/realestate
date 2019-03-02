# title : TERA 
# author : hjy

# library
library(data.table)

# path 
PATH_INPUT = "c:/Users/user/Documents/TERA/input"
PATH_OUTPUT = "c:/Users/user/Documents/TERA/output"

# read data
data = fread(file.path(PATH_INPUT,"TRDB_연체.csv"),fill=T)
head(data)

# 법인전용상품 제외 
data = data[-grep("#법인전용상품",x1),]
head(data)

# ----
# features 
# ----

# 번호
data[,번호:=mapply("[[",strsplit(x1,split = " "),1)]
data[,번호:=gsub("[^0-9]","",번호)]
data[,번호:=as.numeric(번호)]
data.table(table(data$번호,useNA="always"))

# 지역 
data[,지역:=mapply("[[",strsplit(x1,split = " "),2)]
data[,지역:=gsub("[0-9]","",지역)]
data[,지역:=gsub("[A-Z]","",지역)]
data.table(table(data$지역,useNA="always"))

# 지역2
data[,tmp1:=mapply("[[",strsplit(x1,split = " "),3)]
data[,tmp2:=mapply("[[",strsplit(x1,split = " "),4)]
data[,지역2:=paste0(tmp1,"_",tmp2)]
data[,지역2:=mapply("[[",strsplit(지역2,split = "#"),1)]
data[,':='(tmp1=NULL,tmp2=NULL)]
data.table(table(data$지역2,useNA="always"))

# 순위 
data[,순위:=mapply("[[",strsplit(x1,split = "#"),2)]
data[,순위:=gsub("순위","",순위)]
data[,순위:=as.numeric(순위)]
data.table(table(data$순위,useNA="always"))

# 차수 
data[,tmp1:=mapply("[[",strsplit(x1,split = "#"),1)]
data[,tmp2:=gsub("^.* ","",tmp1)]
data[,차수:=gsub("[^0-9차]","",data$tmp2)]
data[,':='(tmp1=NULL,tmp2=NULL)]
data[,차수:=ifelse(차수=="","1차",차수)]
data[,차수:=gsub("차","",차수)]
data[,차수:=as.numeric(차수)]
data.table(table(data$차수,useNA="always"))

# 수익률 
data[,수익률:=mapply("[[",strsplit(x2,"[?]"),1)]
data[,수익률:=gsub("[^0-9.]","",수익률)]
data[,수익률:=as.numeric(수익률)]

# 투자기가
data[,투자기간:=mapply("[[",strsplit(x2,"[?]"),2)]
data[,투자기간:=gsub("투자기간","",투자기간)]
data[,투자기간:=gsub("개월","",투자기간)]
data[,투자기간:=as.numeric(투자기간)]

# 등급 
data[,등급:=mapply("[[",strsplit(x2,"[?]"),3)]
data[,등급:=gsub("등급","",등급)]

# 모집급액
data[,모집금액:=mapply("[[",strsplit(x2,"[?]"),4)]
data[,모집금액:=gsub("모집금액","",모집금액)]
data[,모집금액:=ifelse(모집금액=="1억원","10000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="2억원","20000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="3억원","30000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="4억원","40000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="5억원","50000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="6억원","60000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="7억원","70000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="8억원","80000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="9억원","90000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="10억원","100000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="11억원","110000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="12억원","120000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="13억원","130000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="14억원","140000만원",모집금액)]
data[,모집금액:=ifelse(모집금액=="15억원","150000만원",모집금액)]
data[,모집금액:=gsub("억","",모집금액)]
data[,모집금액:=gsub(",","",모집금액)]
data[,모집금액:=gsub("만원","",모집금액)]
data[,모집금액:=as.numeric(모집금액)]

# 투자인원
data[,투자인원:=mapply("[[",strsplit(x2,"[?]"),5)]
data[,투자인원:=gsub("투자인원","",투자인원)]
data[,투자인원:=gsub("[^0-9.]","",투자인원)]
data[,투자인원:=as.numeric(투자인원)]

# save 
data[,TARGET:="연체"]
write.csv(data,file.path(PATH_OUTPUT,"101_TERA100.csv"),row.names = F)



