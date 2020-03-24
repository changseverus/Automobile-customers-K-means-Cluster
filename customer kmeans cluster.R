library(stats)
library(ggplot2)

dat<-read.table("survey1.csv",header=TRUE,sep=",")

# view the data, run basic EDA
str(dat)
head(dat)
summary(dat)

# divide the customers into 4 clusters
km<-kmeans(dat,centers=4,nstart=10)
summary(km)

# see the metrics of this 4-clusters model
wss<-km$tot.withiness   #smaller the better
bss<-km$betweenss      #larger the better
tss<-wss+bss
ratio<-wss/tss

print(wss,bss,tss,ratio)

dat$cluster<-km$cluster

# plot a scatterplot to see the distribution of different variables
ggplot(dat,aes(x=FAC2,y=FAC3))+geom_point(aes(color=factor(km$cluster)))


# if unsure how many cluster should be set, run sapply to see the ratios of 1~10 clusters
klist<-seq(1:10)
knn<-function(x){
  kms<-kmeans(dat,centers=x,nstart=10)
  ratio<-kms$tot.withiness/(kms$tot.withiness+kms$betweenss)
}

ratios<-sapply(klist,knn)
df<-data.frame(kv=klist,KMratio=ratios)

ggplot(df,aes(x=kv,y=ratios,label=kv,color=ratios))+geom_point(size=5)+geom_text(vjust=2)
