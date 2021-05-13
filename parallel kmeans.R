################################################################
####################### aws로 돌릴 코드. Kmeans
library(data.table)
library(foreach)
library(doParallel)

ex1=fread('Cluster_process1.csv',header = T,encoding = 'UTF-8')

scaled_data=scale(ex1[,-1])

no_cores = detectCores()-1
registerDoParallel(no_cores)
k = 4 #중심점.
results=foreach(i=c(rep(floor(nrow(scaled_data)/k),k-1),
                    floor(nrow(scaled_data)/k) + nrow(scaled_data) %% k) ) %dopar% {
                      kmeans(scaled_data,centers=k,nstart = i)
                    }

temp.vector <- sapply( results, function(result)
{ result$tot.withinss } )
result <- results[[which.min(temp.vector)]]

print(result)
colnames(scaled_data)