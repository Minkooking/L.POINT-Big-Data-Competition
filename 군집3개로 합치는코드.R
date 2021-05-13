## 군집 비교.


library(data.table)
library(dplyr)

getwd()
setwd('C:/Users/김민구/Desktop/제5회 Big Data Competition-Digital Trend Analyzer/제출용 코드/ppt발표 준비/군집분석 그룹')
grp1=fread('group1.csv',encoding = 'UTF-8')
grp2=fread('group2.csv',encoding = 'UTF-8')
grp3=fread('group3.csv',encoding = 'UTF-8')


all.grp=rbind(grp1,grp2,grp3)

uniq.PD_C= all.grp %>% filter(!duplicated(PD_C))

for.clus=uniq.PD_C %>% select(PD_C,cluster)
View(for.clus)

###### brd.power불러오기.
getwd()
brd=fread('brd_power.csv',encoding = 'UTF-8')

olzen <- brd %>% filter(PD_BRA_FIN=="올젠") %>% pull(PD_C) %>% unique()
brd <- brd %>%
  mutate(value=ifelse(PD_C %in% olzen, value+300, value))


brd.pwr=brd %>% 
  group_by(PD_C,PD_BRA_FIN) %>% 
  summarise(brd.sum=sum(value),brd.mean=mean(value))

head(brd.pwr,20)


#### 본데이터.

ex.data=fread('final_weather.csv',encoding = 'UTF-8')


a=ex.data


################################################
v1=female_ratio(a)
v2=nonmember_ratio(a)
v3=age_ratio(a)
v4=month_ratio(a)
v5=hits(a)
v6=time_per_page(a)
v7=recency(a)
v8=wtmean_BUY_AM(a)
v9=mean_ct(a)
v10=Monetary(a)
v11=Frequency(a)
v12=mean_repurchase(a)
v13=weather(a)
v14=div_ppl(a)

merging=join_all(list(v1,v2,v3,v4,v5,v6,v7,v8,v9,
                      v10,v11,v12,v13,v14),by = 'PD_C',type = 'left')
View(merging)

brd.merge=left_join(merging,brd.pwr,by='PD_C')
View(brd.merge)

final.merge=left_join(brd.merge,for.clus,by='PD_C')
summary(final.merge)

classification= a %>% filter(!duplicated(PD_C)) %>% 
  select(PD_C,CLAC1_NM,CLAC2_NM,CLAC3_NM)

real.final.merge=left_join(final.merge,classification,by='PD_C')


View(real.final.merge)


write.csv(real.final.merge,'cluster_final_183day_merge.csv',
          fileEncoding = 'UTF-8',row.names = F)


################################################################################
# 여성회원비중
female_ratio=function(data){
  
  aa = data %>% filter(!is.na(CLNT_GENDER)) %>% 
    select(PD_C,CLNT_ID,CLNT_GENDER) %>% 
    group_by(PD_C) %>% filter(!duplicated(CLNT_ID)) %>% 
    summarise(female.ratio=sum(CLNT_GENDER=='F')/n()) %>% round(4)
  
  return(aa)
}

# 구매자중 비회원비중.
nonmember_ratio=function(data){
  data=a
  aa=data %>% select(PD_C,CLNT_ID,CLNT_GENDER) %>% 
    group_by(PD_C) %>% filter(!duplicated(CLNT_ID)) %>%
    summarise(non.member=sum(is.na(CLNT_GENDER))/n()) %>% round(4)
  
  return(aa)
}


# 연령대별 비중
age_ratio=function(data){
  
  if(("package:dplyr" %in% search())==1) detach('package:dplyr')
  library(plyr)
  suppressMessages(library(dplyr))
  
  
  data$CLNT_AGE=suppressMessages(a$CLNT_AGE %>% as.character() %>% 
                                   revalue(c('10'='10대','20'='20대','30'='30대','40'='40대',
                                             '50'='50대','60'='60대이상','70'='60대이상','80'='60대이상')))
  
  aa=data %>% select(PD_C,CLNT_ID,CLNT_AGE) %>% filter(!(is.na(CLNT_AGE))) %>% 
    group_by(PD_C) %>% filter(!duplicated(CLNT_ID)) %>% 
    summarise(age10=sum(CLNT_AGE=='10대')/n(),
              age20=sum(CLNT_AGE=='20대')/n(),
              age30=sum(CLNT_AGE=='30대')/n(),
              age40=sum(CLNT_AGE=='40대')/n(),
              age50=sum(CLNT_AGE=='50대')/n(),
              age.over60=sum(CLNT_AGE=='60대이상')/n()) %>% round(4)
  
  return(aa)
}


# 월별 비중
month_ratio = function(data){
  
  aa = data %>% group_by(PD_C) %>% 
    summarise(april=sum(MONTH==4)/n(),
              may=sum(MONTH==5)/n(),
              june=sum(MONTH==6)/n(),
              july=sum(MONTH==7)/n(),
              august=sum(MONTH==8)/n(),
              september=sum(MONTH==9)/n()) %>% round(4)
  return(aa)
}


# recency 얼마나 최근에 이 상품이 팔렸는지.
recency = function(data){
  
  aa = data %>% select(PD_C,CLNT_ID,SESS_DT) %>% 
    group_by(PD_C,CLNT_ID) %>% 
    summarise(date.diff=as.numeric(as.Date('2018-10-01')-as.Date(max(SESS_DT)))) %>% 
    group_by(PD_C) %>% 
    summarise(mean.date.diff=mean(date.diff))
  
  return(aa)
}


# 상품별 단가의 가중평균.
wtmean_BUY_AM=function(data){
  
  aa=data %>% select(PD_C,PD_BUY_AM) %>% group_by(PD_C,PD_BUY_AM) %>% 
    summarise(freq=n()) %>% 
    mutate(multiply=PD_BUY_AM*freq) %>% 
    group_by(PD_C) %>% summarise(wtmean.BUY_AM=sum(multiply)/sum(freq))
  
  return(aa)
}

# PD_C별로 한 번 살때 평균 몇개 샀는지.

mean_ct=function(data){
  
  aa=data %>% select(PD_C,PD_BUY_CT) %>% 
    group_by(PD_C) %>% summarise(mean.ct=mean(PD_BUY_CT))
  
  return(aa)
}


# CLNT_ID별 재구매 건수를 모아서 평균낸 것.
mean_repurchase=function(data){
  
  aa=data %>% group_by(PD_C,CLNT_ID) %>% filter(!duplicated(SESS_DT)) %>% 
    summarise(ct=n()) %>% 
    group_by(PD_C) %>% summarise(mean.repurchase=mean(ct))
  
  return(aa)
}

# 분석기간 내 PD_C별 총 판매건수 : Frequency

Frequency=function(data){
  
  aa=data %>% group_by(PD_C) %>% summarise(frequency=n())
  return(aa)
}  

# Monetary: rawdata에서 PD_BUY_AM*PD_BUY_CT 한걸 PD_C별 합.
Monetary=function(data){
  
  aa=data %>% group_by(PD_C) %>% summarise(monetary=sum(PD_BUY_AM*PD_BUY_CT))
  
  return(aa)
}




# PD_C별 TOT_SESS_HR_V / TOT_PAG_VIEW_CT 나누기
time_per_page=function(data){
  
  aa=data %>% group_by(PD_C) %>% 
    summarise(time.per.pag=mean(TOT_SESS_HR_V/TOT_PAG_VIEW_CT))
  return(aa)
}



# PD_C별 HITS_SEQ

hits=function(data){
  
  aa=data %>% group_by(PD_C) %>% summarise(mean.hits=mean(HITS_SEQ))
  
  return(aa)
}



#PD_C별 평균기온, 강수비중 (지역이랑 날짜가 같이 unique한 것.)
weather=function(data){
  
  a=data %>% group_by(PD_C,temperature,rain) %>% distinct(SESS_DT,kor) %>% 
    group_by(PD_C) %>% summarise(mean.temp=mean(temperature),
                                 mean.rain=mean(rain)) %>% round(4)
  return(a)
}


# 얼마나 다양한 사람들이 샀는가?
div_ppl=function(data){
  
  a <- data %>% group_by(PD_C) %>% summarise(div.ppl=length(unique(CLNT_ID)))
  
  return(a)
}