library(data.table)
library(plyr)
library(dplyr)



ex.data=fread('final_weather.csv',encoding = 'UTF-8')

SSdate=ex.data %>% filter(!duplicated(SESS_DT)) %>% 
  select(SESS_DT) %>% arrange(SESS_DT) %>% c()
SSdate=unlist(SSdate$SESS_DT)



final.data=fread('final_for_timeseries.csv',encoding = 'UTF-8')

colnames(final.data)


freq.is.0=final.data %>% filter(frequency==0) %>% select(-mean.date.diff)


freq.is.0$brd.power = 0
freq.is.0$mean.temp = 0
freq.is.0$mean.rain = 0







sell.once=final.data %>% filter(frequency!=0) %>% select(-mean.date.diff)


# 반드시 저장해주어야함.
col_order= c('female.ratio','non.member','age10','age20','age30','age40',
             'age50','age.over60','monetary','frequency','mean.repurchase',
             'mean.rain','div.ppl','brd.power','mean.temp','date','PD_C','cluster')

a=sell.once[,col_order]


scaling=function(x,a=0,b=5){
  
  (((b-a)*(x-min(x)))/(max(x)-min(x)))+a
  
}




# 반드시 저장해주어야함.
col_order= c('female.ratio','non.member','age10','age20','age30','age40',
             'age50','age.over60','monetary','frequency','mean.repurchase',
             'mean.rain','div.ppl','brd.power','mean.temp','date','PD_C','cluster')

dd=NULL

for (i in 1:183){
  
  bb=a %>% filter( date==SSdate[i] ) %>% select( -date,-PD_C,-cluster,-female.ratio,-non.member,-age10,-age20,-age30,-age40,
                                                 -age50,-age.over60) # 적당히 선택.
  for_pd_clust=a %>% filter( date==SSdate[i] ) %>% select( date,PD_C,cluster,female.ratio,non.member,age10,age20,age30,age40,
                                                           age50,age.over60) # 갖다붙이기.
  jk=bb  # 객체 복사. -> 스케일링 돌릴거. if문 때문에 바뀔 수 있어서 넣음.
  vec=NULL # 빈 벡터 만들기.
  
  for (j in 1:ncol(bb)){      # 각 열마다 같은 값으로만 이루어져있는지 확인. (NAN문제.)
    if (sum(duplicated(bb[,j]))+1==length(bb[,j]))
    { 
      
      vec=c(vec,j)  # 1개가 아닐 수 있으니, vec에 계속 저장.
      
      lnm=bb[,-vec] # lnm이라는 객체에 그 열들 뺀거 저장.
      
      jk=lnm # 스케일 돌리는 객체로 저장.
      
    }
  }
  
  cc=apply( jk, 2, scaling )
  
  
  
  if (ncol(cc)!=7){
    
    bb[,vec]=0 # 아까 빼준거 밖에서 값으로 0 넣음.
    
    col.nm=names(bb[vec])
    
    cc=cbind(cc,bb[vec]) # 나머지 정상인 애들(스케일링된거)이랑 합침.
    
  }
  
  
  cc=cbind(cc,for_pd_clust)
  
  cc$mean.temp=cc$mean.temp^2
  
  # 다음 시행 i+1에 j 순서가 뒤바뀌면 밑에 rbind를 못함. 그래서 j 순서 고정해주어야 함.
  
  ff=cc[col_order] # 열 순서 바꿔주자.
  
  dd=rbind(dd,ff) # rbind로 붙여주자.
}



scaled_final_data=rbind(dd,freq.is.0)



sad=function(rep){
  ####################           
  g1=scaled_final_data %>% filter(cluster==1) %>% select(PD_C)
  
  g1_sam=sample(g1$PD_C,rep,replace = F)
  
  
  LPOindex1=scaled_final_data %>% filter(PD_C %in% g1_sam) %>% group_by(PD_C,date) %>% summarise(L_index=sum(female.ratio,non.member,age10,age20,age30,age40,age50,age.over60),
                                                                                                 P_index=sum(monetary,frequency,mean.repurchase,div.ppl,brd.power),
                                                                                                 O_index=sum(mean.temp,mean.rain),
                                                                                                 LPOI=sum(L_index,P_index,O_index))
  
  LPOI_from_g1=LPOindex1 %>% group_by(PD_C) %>% mutate(cummul=cumsum(LPOI))
  LPOI_from_g1$cluster=1
  #########################
  #####################
  g2=scaled_final_data %>% filter(cluster==2) %>% select(PD_C)
  
  g2_sam=sample(g2$PD_C,rep,replace = F)
  
  
  LPOindex2=scaled_final_data %>% filter(PD_C %in% g2_sam) %>% group_by(PD_C,date) %>% summarise(L_index=sum(female.ratio,non.member,age10,age20,age30,age40,age50,age.over60),
                                                                                                 P_index=sum(monetary,frequency,mean.repurchase,div.ppl,brd.power),
                                                                                                 O_index=sum(mean.temp,mean.rain),
                                                                                                 LPOI=sum(L_index,P_index,O_index))
  
  
  LPOI_from_g2=LPOindex2 %>% group_by(PD_C) %>% mutate(cummul=cumsum(LPOI))
  LPOI_from_g1$cluster=2
  ########################3
  #########################
  
  g3=scaled_final_data %>% filter(cluster==3) %>% select(PD_C)
  
  g3_sam=sample(g3$PD_C,rep,replace = F)
  
  
  LPOindex3=scaled_final_data %>% filter(PD_C %in% g3_sam) %>% group_by(PD_C,date) %>% summarise(L_index=sum(female.ratio,non.member,age10,age20,age30,age40,age50,age.over60),
                                                                                                 P_index=sum(monetary,frequency,mean.repurchase,div.ppl,brd.power),
                                                                                                 O_index=sum(mean.temp,mean.rain),
                                                                                                 LPOI=sum(L_index,P_index,O_index))
  
  
  LPOI_from_g3=LPOindex3 %>% group_by(PD_C) %>% mutate(cummul=cumsum(LPOI))
  LPOI_from_g3$cluster=3
  ########################3
  #########################
  
  g4=scaled_final_data %>% filter(cluster==4) %>% select(PD_C)
  
  g4_sam=sample(g4$PD_C,rep,replace = F)
  
  
  LPOindex4=scaled_final_data %>% filter(PD_C %in% g4_sam) %>% group_by(PD_C,date) %>% summarise(L_index=sum(female.ratio,non.member,age10,age20,age30,age40,age50,age.over60),
                                                                                                 P_index=sum(monetary,frequency,mean.repurchase,div.ppl,brd.power),
                                                                                                 O_index=sum(mean.temp,mean.rain),
                                                                                                 LPOI=sum(L_index,P_index,O_index))
  
  
  LPOI_from_g4=LPOindex4 %>% group_by(PD_C) %>% mutate(cummul=cumsum(LPOI))
  LPOI_from_g4$cluster=4
  ########################3
  
  mm=rbind(LPOI_from_g1,LPOI_from_g2,LPOI_from_g3,LPOI_from_g4)
  
  
  #master=fread("제5회 Big Data Competition-분석용데이터-06.Master.csv",encoding = "UTF-8")
  
  bnm=left_join(mm,master[,c(1,2)],by='PD_C')
}

output=sad(1000)


write.csv(output,'to5_4.csv',fileEncoding = 'UTF-8',row.names = F)




#################################





scaled_final_data %>% filter(PD_C==733421) %>% summarise(L_index=sum(female.ratio,non.member,age10,age20,age30,age40,age50,age.over60),
                                                         P_index=sum(monetary,frequency,mean.repurchase,div.ppl,brd.power),
                                                         O_index=sum(mean.temp,mean.rain),
                                                         LPOI=sum(L_index,P_index,O_index))






write.csv(scaled_final_data,'scaled_final_ts_data.csv',fileEncoding = 'UTF-8',row.names = F)



