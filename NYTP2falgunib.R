#install.packages("doBy")
#install.packages("dplyr")
#install.packages("survival")
#install.packages("ggplot2")
library(survival)
library(doBy)
library(ggplot2)
library(dplyr)

data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))

head(data1)
#Part 1: Age category
data1$agecat <-cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
summary(data1)
#Summary Functions
s <- function(x){c(length = length(x),min = min(x),mean = mean(x), max = max(x), median = median(x))} 
summaryBy(Age~agecat, data =data1, FUN=s)
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat,data =data1)
data1$hasimps <-cut(data1$Impressions,c(-Inf,0,Inf)) 
summaryBy(Clicks~hasimps, data =data1, FUN=s) 
ggplot(subset(data1, Impressions>0), aes(x=Clicks/Impressions, colour=agecat)) + geom_density()

#Plot Signed_In & Not Signed_in
data_l <- subset(data1,data1$Signed_In == 0)
ggplot(subset(data_l, Clicks>0), aes(x=Clicks/Impressions, colour=agecat)) + geom_density()

#Plot CTR-age without signed in
ggplot(data=data1, aes(x=agecat, y=Clicks, fill=agecat)) + geom_bar(stat="identity") + theme_bw()
data1$agecat[data1$Signed_In == 0] = NA
data1$Gender[data1$Signed_In == 0] = NA
summary(data1)
#Plot CTR-age with signed in
ggplot(data=data1, aes(x=agecat, y=Clicks, fill=agecat)) + geom_bar(stat="identity") + theme_bw()

#Summarize data & plot count of clicks
data2 = na.omit(subset(data1, Impressions>0)) %>% group_by(agecat) %>% summarise(Impressions = sum(Impressions), Clicks = sum(Clicks))
ggplot(data=data2, aes(x=agecat, y=Clicks/Impressions, fill=agecat)) + geom_bar(stat="identity") + theme_bw()

#Density Plots
ggplot(subset(data1, Impressions>0), aes(x=Clicks/Impressions, colour=agecat)) + geom_density()
ggplot(subset(data1, Clicks>0), aes(x=Clicks/Impressions, colour=agecat)) + geom_density()
ggplot(subset(data1, Clicks>0), aes(x=Clicks, colour=agecat)) + geom_density()
#First graph histogram impressions with age
ggplot(data1, aes(x=Impressions, fill=agecat)) +geom_histogram(binwidth=1)
#Second graph boxplot
ggplot(data1, aes(x=agecat, y=Impressions, fill=agecat)) +geom_boxplot()
#Weird box plot
ggplot(subset(data1, Clicks>0), aes(x=agecat, y=Clicks,fill=agecat)) + geom_boxplot()


#Gender vs Clicks

data1$Signed_In = factor(data1$Signed_In)
data1$Gender = factor(data1$Gender, levels=c(0,1), labels = c("Female", "Male"))
data18 <- subset(data1, Age <18)
data1188 <-subset(data18, Gender != "NA")
ggplot(subset(data1188, Clicks>0), aes(x=Clicks/Impressions, colour=Gender)) + geom_density()


# 2b create categories
data1$clickcat[data1$Impressions==0] <- "NoImps"
data1$clickcat[data1$Impressions >0] <- "Imps"
data1$clickcat[data1$Clicks >0] <- "Clicks"
data1$clickcat = factor(data1$clickcat)
# Convert the column to a factor
head(data1)
#look at levels
clen <- function(x){c(length(x))} 
etable<-summaryBy(Impressions~clickcat+Gender+agecat,data = data1, FUN=clen)

#Count gap between males & females
agg <- function(x){c(length(x), min(x), mean(x), max(x), sum(x))}
agg_names = c("length", "min", "mean", "max", "sum")
summaryBy(Impressions~agecat, data=data1, FUN=agg, fun.name=agg_names)
summaryBy(Clicks~agecat, data=data1, FUN=agg, fun.name=agg_names)

signed = subset(data1, data1$Signed_In==1)
ggplot(signed, aes(x=Age, fill=Gender))+ geom_histogram(binwidth=1)

