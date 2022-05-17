#####data mining----CHD
library(mice)
library(MASS)
library(ggplot2)
library(dplyr)
library(DMwR)
library(grid)

#####load data
setwd("C:/Users/20833/Desktop/Êý¾ÝÍÚ¾ò/Project")
data<-read.csv("framingham.csv",header=T)

#####deal with missing values
md.pattern(data[!complete.cases(data),])
#missing values:
#heartRate:1,BMI:19,cigersPerDay:29
#totChol:50,BPMeds:53,education:105,glucose:388

##heartRate:impute by median
summary(data[,'heartRate'])
data[is.na(data$heartRate),"heartRate"]<-median(data$heartRate,na.rm=T)

##BPMeds:fisher's lda
data$BPMeds<-as.factor(data$BPMeds)
table(data$BPMeds,exclude=(useNA="ifany"))
data_BP<-data[!is.na(data$BPMeds),c("sysBP","diaBP","BPMeds")]
colMeans(data_BP[data_BP$BPMeds==1,1:2])
colMeans(data_BP[data_BP$BPMeds==0,1:2])
p1<-ggplot(data=data_BP,aes(x=as.factor(BPMeds),y=sysBP,group=BPMeds,fill=BPMeds))+geom_boxplot()+xlab("BPMeds")
p2<-ggplot(data=data_BP,aes(x=as.factor(BPMeds),y=diaBP,group=BPMeds,fill=BPMeds))+geom_boxplot()+xlab("BPMeds")
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=1,ncol=2)))
vplayout <- function(x,y){viewport(layout.pos.row = x,layout.pos.col = y)}
print(p1,vp=vplayout(1,1))
print(p2,vp=vplayout(1,2))

ggplot(data=data_BP,aes(x=sysBP,y=diaBP,colour=as.factor(BPMeds)))+geom_point()


cls<-lda(BPMeds~sysBP+diaBP,data=data_BP)
table(data_BP$BPMeds,predict(cls,data=data_BP)$class)
data[is.na(data$BPMeds),"BPMeds"]<-predict(cls,data[is.na(data$BPMeds),c("sysBP","diaBP")])$class
any(is.na(data$BPMeds))

##BMI totChol glucose:knn+CV for choosing k;
data_knn<-data[,c("age","sysBP","diaBP","heartRate","BMI","totChol","glucose")]

#choose k:
data_knn_complete<-data_knn[complete.cases(data_knn),]
set.seed(2020)
data_knn_complete$col<-sample(dim(data_knn_complete)[1])%%5+1
MSE<-matrix(0,ncol=1,nrow=200)
for(k in 1:50){
  for(i in 1:5){
    tmp<-data_knn_complete
    std<-tmp[tmp$col==i,c("BMI","totChol","glucose")]
    tmp[tmp$col==i,c("BMI","totChol","glucose")]<-NA
    tmp[,-8]<-knnImputation(tmp[,-8],k=k,meth="weighAvg")
    MSE[k,1]<-MSE[k,1]+sum((std-tmp[tmp$col==i,c("BMI","totChol","glucose")])^2)
  }
}

#knn impute
data[,c("age","sysBP","diaBP","heartRate","BMI","totChol","glucose")]<-
  knnImputation(data[,c("age","sysBP","diaBP","heartRate","BMI","totChol","glucose")],k=20,meth="weighAvg")
data$BMI<-round(data$BMI,2)
data$totChol<-round(data$totChol,0)
data$glucose<-round(data$glucose,0)

##cigersPerDay:median
sum(data$currentSmoker==0 & is.na(data$cigsPerDay))
sum(data$currentSmoker==0 & data$cigsPerDay!=0)
summary(data$cigsPerDay)
summary(data[data$cigsPerDay>0,]$cigsPerDay)
data[is.na(data$cigsPerDay),"cigsPerDay"]<-20

##education:mode
table(data$education,data$TenYearCHD)/rowSums(table(data$education,data$TenYearCHD))
chisq.test(table(data$education,data$TenYearCHD))
data[is.na(data$education),"education"]<-1

any(is.na(data))

write.csv(data,"preprocess.csv",row.names=F)













