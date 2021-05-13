# 병렬 PAM

data=fread('final_by_PD_C.csv',encoding = 'UTF-8',header = T)

clust_data=data %>% select(PD_C,mean.hits,time.per.pag)
head(clust_data)

scaled_data=scale(clust_data[,-1])
head(scaled_data)



library(parallel)
library(foreach)
library(doParallel)

no_cores = detectCores()-1
myCluster <- makeCluster(no_cores)
doParallel::registerDoParallel(myCluster)


range=10; my.range=10

dissimilarity=foreach(i = 2:range,
                      .packages = c('cluster'),
                      .combine = rbind) %dopar% {
  mypam=pam(scaled_data,k=i)
  
  c(within=max(mypam$clusinfo[,3]),between=max(mypam$clusinfo[,5]))
}
rownames(dissimilarity)=2:range
dissimilarity

par(mfrow=c(1,2))
plot(x=1:my.range, xlab = '# of K', y=c(NA,dissimilarity[,1]), ylab='', type = 'o'
     , main='Within Cluster Dissimilarity')
plot(x=1:my.range, xlab = '# of K', y=c(NA,dissimilarity[,2]), ylab = '', type='o'
     ,main = 'Outside Cluster Dissimilarity')

stopImplicitCluster() # 병렬처리 끝나면 실행해주기.

