library(DMwR)
library(randomForest)
library(ggplot2)
library(MASS)
library(fmsb)
library(grid)

setwd("C:/Users/20833/Desktop/�����ھ�/Project")
train<-read.csv("train with smote.csv",header=T)
data<-read.csv("preprocess.csv",header=T)

data<-data[,-4]

for(i in c(1,3,5,6,7,8,15)){
  train[,i]<-as.factor(train[,i])
  data[,i]<-as.factor(data[,i])
}

#####1.score by RF
set.seed(2020)
fit_rf<-randomForest(TenYearCHD~.,data=train,proximity=T,importance=T,mtry=3,ntree=550)

pred<-round(100*predict(fit_rf,data,type = "prob")[,1],0)
out<-as.data.frame(data$TenYearCHD)
out$score<-pred
colnames(out)<-c("CHD","score")
summary(out[out$CHD==0,"score"])
summary(out[out$CHD==1,"score"])

ggplot(data=out,aes(x=CHD,y=score,fill=CHD))+geom_boxplot()

out$score_d<-as.numeric(out$score>60)
table(out$CHD,out$score_d)


score<-function(male=NA,age=NA,education=NA,cigsPerDay=NA,BPMeds=NA,prevalentStroke=NA,
                prevalentHyp=NA,diabetes=NA,totChol=NA,sysBP=NA,diaBP=NA,
                BMI=NA,heartRate=NA,glucose=NA){
  sc<-data.frame(male=male,age=age,education=education,cigsPerDay=cigsPerDay,
                 BPMeds=BPMeds,prevalentStroke=prevalentStroke,prevalentHyp=prevalentHyp,
                 diabetes=diabetes,totChol=totChol,sysBP=sysBP,diaBP=diaBP,
                 BMI=BMI,heartRate=heartRate,glucose=glucose)
  for(i in c(1,3,5,6,7,8)){
    sc[,i]<-as.factor(sc[,i])
  }
  knn<-rbind(data[,-15],sc)
  if(!complete.cases(sc)){
    set.seed(2020)
    knn<-knnImputation(knn,k=5)
  }
  pred<-predict(fit_rf,knn[4241,],type = "prob")[,1]
  return(round(100*pred,0))
}

score(male=1,age=20,education=4,cigsPerDay=0,BPMeds=0,prevalentStroke=0,
      prevalentHyp=0,diabetes=0,BMI=27)

score(male=1,age=20,education=4,cigsPerDay=0,BPMeds=0,prevalentStroke=0,
                  prevalentHyp=0,diabetes=0,sysBP=125,diaBP=75,
                  BMI=27,heartRate=80)

#####2.radar graph
pc<-princomp(train[,c("sysBP","diaBP")])
summary(pc,loadings=T)
train$BP<-as.matrix(train[,c("sysBP","diaBP")])%*%pc$loadings[,1]
data$BP<-as.matrix(data[,c("sysBP","diaBP")])%*%pc$loadings[,1]

train$BMI_dis<-as.numeric((train$BMI>=24.4))
train$BMI_dis<-as.factor(train$BMI_dis)
data$BMI_dis<-as.numeric((data$BMI>=24.4))
data$BMI_dis<-as.factor(data$BMI_dis)

fit_lr<-stepAIC(glm(TenYearCHD~male+age+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+diabetes+totChol+heartRate+ 
               glucose+BP+BMI_dis,data=train,family=binomial(link="logit")))
summary(fit_lr)


radar<-function(male=NA,age=NA,cigsPerDay=NA,BPMeds=NA,prevalentStroke=NA,
                prevalentHyp=NA,diabetes=NA,BP=NA,BMI_dis=NA,heartRate=NA,glucose=NA){
  sc<-data.frame(male=male,age=age,cigsPerDay=cigsPerDay,BPMeds=BPMeds,
                 prevalentStroke=prevalentStroke,prevalentHyp=prevalentHyp,
                 diabetes=diabetes,heartRate=heartRate,glucose=glucose,BP=BP,BMI_dis=BMI_dis)
  for(i in c(1,4,5,6,7,11)){
    sc[,i]<-as.factor(sc[,i])
  }
  knn<-rbind(data[,c(1,2,4,5,6,7,8,13,14,16,17)],sc)
  if(!complete.cases(sc)){
    set.seed(2020)
    knn<-knnImputation(knn,k=5)
  }
  sc<-knn[4241,]
  
  tmp<-train
  tmp$score1<-0.009962*tmp$BP+2.840240*as.numeric(tmp$BPMeds)
  tmp$score2<-0.016812*tmp$cigsPerDay
  tmp$score3<-(-0.015534)*tmp$heartRate+0.013776*tmp$glucose+0.128391*as.numeric(tmp$BMI_dis)
  tmp$score4<-2.623796*as.numeric(tmp$prevalentStroke)+0.548733*as.numeric(tmp$prevalentHyp)+2.124327*as.numeric(tmp$diabetes)
  
  sc$score1<-0.009962*sc$BP+2.840240*as.numeric(sc$BPMeds)
  sc$score2<-0.016812*sc$cigsPerDay
  sc$score3<-(-0.015534)*sc$heartRate+0.013776*sc$glucose+0.128391*as.numeric(sc$BMI_dis)
  sc$score4<-2.623796*as.numeric(sc$prevalentStroke)+0.548733*as.numeric(sc$prevalentHyp)+2.124327*as.numeric(sc$diabetes)
  
  
  
  sc$s1<-40+60/(quantile(tmp$score1,0.001)-quantile(tmp$score1,0.999))*(sc$score1-quantile(tmp$score1,0.999))
  sc$s2<-40+60/(quantile(tmp$score2,0.001)-quantile(tmp$score2,0.999))*(sc$score2-quantile(tmp$score2,0.999))
  sc$s3<-40+60/(quantile(tmp$score3,0.001)-quantile(tmp$score3,0.999))*(sc$score3-quantile(tmp$score3,0.999))
  sc$s4<-40+60/(quantile(tmp$score4,0.001)-quantile(tmp$score4,0.999))*(sc$score4-quantile(tmp$score4,0.999))
  
  sc<-sc[,c("s1","s2","s3","s4")]
  colnames(sc)<-c("BP","Cigs","health","illness")
  
  return(sc)
  
}


d<-radar(male=1,age=21,cigsPerDay=0,BPMeds=0,prevalentStroke=0,
                prevalentHyp=0,diabetes=0,BP=NA,BMI_dis=1,heartRate=80,glucose=NA)

d<-data.frame(car=c("BP","Cigs","health","illness"),id=c(1:4),value=c(75.6,86.4,92.0,100))
AddRow<-c(NA,nrow(d)+1,d[1,ncol(d)])
a<-rbind(d,AddRow)

p1<-ggplot(data=a,aes(x=id,y=value))+geom_polygon(color="black",fill="#E41A1C",alpha=0.1)+
  geom_point(size=5,shape=21,color="black",fill="#E41A1C")+coord_polar()+ylim(40,100)+
  scale_x_continuous(breaks=d$id,labels=d$car)+theme_light()+theme(axis.title.x = element_text(size=11,color="black"))+ggtitle("no CHD")

p2<-ggplot(data=a,aes(x=id,y=value))+geom_polygon(color="black",fill="#E41A1C",alpha=0.1)+
  geom_point(size=5,shape=21,color="black",fill="#E41A1C")+coord_polar()+ylim(40,100)+
  scale_x_continuous(breaks=d$id,labels=d$car)+theme_light()+theme(axis.title.x = element_text(size=11,color="black"))+ggtitle("no CHD")

grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=1,ncol=2)))
vplayout <- function(x,y){viewport(layout.pos.row = x,layout.pos.col = y)}
print(p1,vp=vplayout(1,1))
print(p2,vp=vplayout(1,2))





