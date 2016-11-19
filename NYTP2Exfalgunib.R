#Data for month
j=1
for(i in 1:31)
{
  data1 <- read.csv(url(paste("http://stat.columbia.edu/~rachel/datasets/nyt",i,".csv",sep="")))
  data1$agecat <-cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
  data1$day=j
  if(j==1) data<-data1
  else 
  {
    data2<-rbind(data,data1)
    data<-data2
  }
  head(data)
  j=j+1
}

summary(data2)

s <- function(x){c(length = length(x),min = min(x),mean = mean(x), max = max(x), median = median(x))} 
summaryBy(Age~agecat, data =data2, FUN=s)
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat,data =data1)
#First graph histogram impressions with age
ggplot(data2, aes(x=Impressions, fill=agecat)) +geom_histogram(binwidth=1)

ggplot(data2, aes(x=agecat, y=Impressions, fill=agecat)) +geom_boxplot()

#day vs count
ggplot(subset(data2, Clicks>0), aes(x=Clicks/Impressions, colour=day)) + geom_density()
#density distribution

data1.signedin<-data2[data1$Signed_In==1,]
data1.notsignedin<-data2[data1$Signed_In==0,]
ggplot(subset(data1.signedin, Clicks>0), aes(x=Clicks/Impressions,colour=agecat))+geom_density()
ggplot(subset(data1.notsignedin, Clicks>0), aes(x=Clicks/Impressions,colour=agecat))+geom_density()
