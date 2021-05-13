library(data.table)
library(plyr)
library(dplyr)


ex.data=fread('final_weather.csv',encoding = 'UTF-8')

date=ex.data %>% filter(!duplicated(SESS_DT)) %>% 
  select(SESS_DT) %>% arrange(SESS_DT) %>% c()
date=unlist(date$SESS_DT)

# 유니크한 PD_C
unique.PD_C=sort(unique(ex.data$PD_C))


# data 가볍게하기
ex.data <- ex.data %>% select(-SESS_SEQ,-HITS_SEQ,-MONTH,-WKDAY,-TOT_PAG_VIEW_CT,-TOT_SESS_HR_V,
                              -DVC_CTG_NM,-ZON_NM,-CITY_NM,-PD_BRA_NM,-PD_NM,
                              -CLAC1_NM,-CLAC2_NM,-CLAC3_NM,-PD_ADD_NM)

# 구매기록 없는 데이터에 서울 날씨.
empty_data_for_weather <- ex.data %>% filter(kor=='서울') %>%
  select(SESS_DT,temperature,rain) %>%
  filter(!duplicated(SESS_DT)) %>% arrange(SESS_DT) # arrange 필수.



zero.mat=data.frame(PD_C=0,female.ratio=0,
                    non.member=0,age10=0,age20=0,age30=0,age40=0,
                    age50=0,age.over60=0,mean.date.diff=0,
                    monetary=0,frequency=0,mean.repurchase=0,mean.temp=0,mean.rain=0,div.ppl=0,
                    date=0,row.names = NULL)



################################# 병렬처리.
library(parallel)
library(foreach)
library(doParallel)

no_cores = detectCores()-1    
myCluster <- makeCluster(no_cores)
doParallel::registerDoParallel(myCluster)
snow::clusterExport(myCluster,list=c('ex.data','date','unique.PD_C',
                                     'empty_data_for_weather','zero.mat'))


#######################
results <- foreach (i = unique.PD_C,
                    .packages = c('data.table','plyr','dplyr'),.multicombine = T) %:%
  foreach (j = 1:183,.combine='rbind') %dopar% {
    
    a=ex.data %>% filter(PD_C==i & (SESS_DT == date[j]))
    
    
    
    if (nrow(a)==0) { # 해당하는 행의 개수가 0일 때.
      
      zero.mat$PD_C=i
      zero.mat$date=date[j]
      zero.mat$mean.temp=empty_data_for_weather$temperature[j]
      zero.mat$mean.rain=empty_data_for_weather$rain[j]
      
      zero.mat
      
    } else {  # nrow(a)!=0. 해당하는 행의 개수가 0이 아닐 때. 넣을게 있을 때.
      
      # 행은 있지만, 여성회원 및 연령별 비율 계산하기위해 NA 제거했는데,
      # 행이 하나도 없을 때.
      if ( nrow(a %>% filter(!is.na(CLNT_GENDER)))==0 ) {
        
        
        zero.mat$PD_C=i
        zero.mat$date=date[j]
        zero.mat$mean.temp=empty_data_for_weather$temperature[j]
        zero.mat$mean.rain=empty_data_for_weather$rain[j]
        
        zero.mat
        
      } else { # NA지워도 행이 남아있다면? -> 그냥 계산. 
        
        v1=female_ratio(a)
        v2=nonmember_ratio(a)
        v3=age_ratio(a)
        v4=recency(a)
        v5=Monetary(a)
        v6=Frequency(a)
        v7=mean_repurchase(a)
        v8=weather(a)
        v9=div_ppl(a)
        
        merging=join_all(list(v1,v2,v3,v4,v5,v6,v7,v8,v9),by = 'PD_C',type = 'left')
        merging$date=date[j]  
        
        merging
        
      } # NA지워도 남았니마니..
      
    } # 조건문 종료
    
  } 


stopCluster(myCluster)


df_results=ldply (results, data.frame) # 데이터프레임 전환.
# df_results


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

