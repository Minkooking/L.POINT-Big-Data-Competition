library(data.table)
detach('package:dplyr')
library(plyr)
library(dplyr)
library(ggplot2)

getwd()

setwd('C:/Users/김민구/Desktop/제5회 Big Data Competition-Digital Trend Analyzer/제출용 코드/ppt발표 준비/군집분석 그룹')

data=fread('cluster_final_183day_merge.csv',encoding = 'UTF-8')

data$cluster=as.factor(data$cluster)
a=data$cluster

data$cluster=a %>% revalue(c('1'='우물형',
                             '2'='쾌속형',
                             '3'='탐색형'))
master=fread("제5회 Big Data Competition-분석용데이터-06.Master.csv",
             encoding = "UTF-8")

data=left_join(data,master[,1:2],by='PD_C')

bv=manova(cbind(mean.hits,time.per.pag)~cluster,data=data)
summary(bv)

data %>% filter(cluster=='쾌속형') %>% filter(!duplicated(CLAC2_NM)) %>% 
  select(frequency,PD_C,PD_NM,CLAC2_NM) %>% 
  View()

View(data %>% select(PD_C,PD_NM,cluster,CLAC2_NM))

# 기본 군집
ggplot(data,aes(x=mean.hits,y=time.per.pag,
                color=cluster))+geom_point()

# 여성회원 비율.
ggplot(data,aes(fill=cluster,x = mean.repurchase^2))+
  geom_density()+xlim(0,1.7)

data %>% filter(cluster=='small_fast') %>% arrange(desc(mean.repurchase^2)) %>% head(1)

ggplot(data,aes(x=cluster,y = div.ppl))+
  geom_boxplot()

ggplot(data,aes(fill=cluster,x = frequency))+
  geom_histogram()


data %>% group_by(cluster) %>% 
    summarise(mean_div.ppl=mean(div.ppl))
## 소수의 정보를 천천히 본 집단이 여성비율이 적음.
colnames(data)
classi.clust=data %>% group_by(CLAC2_NM) %>% 
  summarise(small.slow=sum(cluster=='우물형')/n(),
            small.fast=sum(cluster=='쾌속형')/n(),
            many.fast=sum(cluster=='탐색형')/n())

vvv=colnames(classi.clust[2:4])[apply(classi.clust[2:4],1,which.max)]
classi.clust=cbind(classi.clust,vvv)

View(classi.clust)

data %>%
  select(PD_C,cluster,PD_NM,frequency,female.ratio,age30,CLAC2_NM,div.ppl) %>%
  arrange(desc(female.ratio),desc(age30),desc(frequency)) %>% 
  View()

data %>% group_by(cluster) %>% 


data$PD_C[data$frequency==data$div.ppl]
View(Product %>% filter(PD_C==844388))
data %>% filter(PD_C==844388)
sum(data$frequency<data$div.ppl)






master %>% filter(CLAC2_NM=='인라인/스케이트보드/킥보드') %>% View()

data %>% group_by(cluster) %>% 
  summarise(female=mean(mean.hits))

data %>% group_by(cluster) %>% 
  summarise(female=mean(time.per.pag))

data %>% group_by(cluster) %>% 
  summarise(female=mean(mean.repurchase^2))

data %>% group_by(cluster) %>% 
  summarise(female=mean(wtmean.BUY_AM)) %>% as.data.frame()

data %>% group_by(cluster) %>% 
  summarise(female=mean(div.ppl))

data %>% group_by(cluster) %>% 
  summarise(female=mean(non.member))

data %>% group_by(cluster) %>% # 의미별로.
  summarise(female=mean(mean.date.diff)) 

data %>% group_by(cluster) %>% 
  summarise(female=mean(mean.ct^2))

data %>% group_by(cluster) %>% 
  summarise(ff=mean(monetary))
colnames(data)

data %>% group_by(cluster) %>% 
  summarise(female=mean(brd.sum/183))

data %>% group_by(cluster) %>% 
  summarise(female=mean(brd.mean)) %>% as.data.frame()

data %>% group_by(cluster) %>% 
  summarise(female=mean(frequency)) %>% as.data.frame()

data %>% group_by(cluster) %>% 
  summarise()

ggplot(data,aes(x=frequency,y=..density..,
                fill=cluster))+
  geom_histogram(binwidth = 10,position = 'dodge')+
  xlim(0,250)

data %>% filter(frequency>250) %>%
  select(cluster,PD_C,PD_NM,frequency,CLAC2_NM) %>%
  arrange(cluster,desc(frequency)) %>% 
  View()

unique(master$CLAC3_NM) %>% View()

data %>% summary(frequency)
View(master)
summary(data$frequency)


data %>% group_by(cluster) %>% 
  summarise(female=median(mean.temp))

data %>% group_by(cluster) %>% 
  summarise(female=median(mean.rain))


data %>% group_by(cluster) %>% 
  summarise(mean10=mean(age10),
            mean20=mean(age20),
            mean30=mean(age30),
            mean40=mean(age40),
            mean50=mean(age50),
            mean670=mean(age.over60)) %>% View()

##############################################
# 연령대별??

classi.age=data %>% group_by(CLAC2_NM) %>% 
  summarise(mean10=mean(age10),
            mean20=mean(age20),
            mean30=mean(age30),
            mean40=mean(age40),
            mean50=mean(age50),
            mean670=mean(age.over60))
View(classi.age)

vvv=colnames(classi.age[2:7])[apply(classi.age[2:7],1,which.max)]
##########################
head(data,1)
l.index=data %>% select(female.ratio,non.member,frequency,div.ppl)

cor(l.index)







summary(data$div.ppl)

ggplot(data,aes(x=div.ppl))+
  geom_histogram(binwidth = 5,
                 color='black',fill='steelblue2')+
  xlim(10,150)+  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

data %>% filter(div.ppl==4229) %>% select(PD_C,PD_NM,PD_BRA_FIN)
######################
summary(data$mean.temp)

ggplot(data,aes(x=mean.temp))+
  geom_histogram(binwidth = 1/3,
                 color='black',fill='steelblue2')+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())


########################
summary(data$mean.rain)

ggplot(data,aes(x=mean.rain))+
  geom_histogram(binwidth = 1/3,
                 color='black',fill='steelblue2')+
  theme_bw()+xlim(0,30)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
########################
summary(data$brd.sum)

ggplot(data,aes(x=brd.mean))+
  geom_histogram(binwidth = 1/3,
                 color='black',fill='steelblue2')+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

data %>% filter(brd.sum==1510545)


############## 단가 unique한 PD_C로 보기 ##
vxx=fread('final_weather.csv',encoding = 'UTF-8')

asdf=vxx %>% group_by(PD_C) %>% 
  filter(!duplicated(PD_BUY_AM)) %>% 
  summarise(mean.danka=mean(PD_BUY_AM))

asdf %>% filter(PD_C==7)
  
#write.csv(asdf,'danka.csv',fileEncoding = 'UTF-8',row.names = F)

setwd('C:/Users/김민구/Desktop/제5회 Big Data Competition-Digital Trend Analyzer/제출용 코드/ppt발표 준비/군집분석 그룹')

aaaa=fread('danka.csv',encoding = 'UTF-8')

last.data=left_join(data,aaaa,by='PD_C')

last.data %>% group_by(cluster) %>% summarise(aa=mean(mean.danka))

last.data %>% select(cluster,PD_C,mean.danka,
                     wtmean.BUY_AM,CLAC2_NM,PD_NM) %>% 
  View()

ggplot(last.data,aes(x=mean.danka,y))

u1=last.data %>% filter(cluster=='우물형') 
u2=last.data %>% filter(cluster=='쾌속형') 
u3=last.data %>% filter(cluster=='탐색형') 
summary(u1$mean.danka)
summary(u2$mean.danka)
summary(u3$mean.danka)

sqrt(var(u1$mean.danka))
sqrt(var(u2$mean.danka))
sqrt(var(u3$mean.danka))

#############
