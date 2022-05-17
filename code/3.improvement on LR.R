library(MASS)
library(pROC)
library(caret)
library(ggplot2)

setwd("C:/Users/20833/Desktop/Êý¾ÝÍÚ¾ò/Project")
train<-read.csv("train with smote.csv",header=T)
test<-read.csv("test.csv",header=T)

for(i in c(1,3,5,6,7,8,15)){
  train[,i]<-as.factor(train[,i])
  test[,i]<-as.factor(test[,i])
}


####improvement on LR
fit_LR<-stepAIC(glm(TenYearCHD~.,data=train,family=binomial(link="logit")))
summary(fit_LR)

##BP
cor(train$sysBP,train$diaBP)
pred_LR_train<-predict(object=fit_LR,newdata=train[,-15],type="response")
roc_LR_train<-roc(train$TenYearCHD,pred_LR_train)
plot(roc_LR_train,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE) #0.861
opt_LR<-coords(roc=roc_LR_train,x="best",ret="threshold",best.method="closest.topleft")
pred_LR_train<-ifelse(pred_LR_train<opt_LR,0,1)
confusionMatrix(data=as.factor(pred_LR_train),reference=as.factor(train$TenYearCHD))
#0.7782,0.804,0.7519

pred_LR_test<-predict(object=fit_LR,newdata=test[,-15],type="response")
roc_LR_test<-roc(test$TenYearCHD,pred_LR_test)
plot(roc_LR_test,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE) #0.731
pred_LR_test<-ifelse(pred_LR_test<opt_LR,0,1)
confusionMatrix(data=as.factor(pred_LR_test),reference=as.factor(test$TenYearCHD))
#0.7759,0.8310,0.4603

##1.delete diaBP
fit_BP1<-stepAIC(glm(TenYearCHD~.,data=train[,-11],family=binomial(link="logit")))
summary(fit_BP1)
pred_BP1_train<-predict(fit_BP1,newdata=train[,-15],type="response",quiet=T)
roc_BP1_train<-roc(train$TenYearCHD,pred_BP1_train)
plot(roc_BP1_train,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE) #0.861
opt_BP1<-coords(roc=roc_BP1_train,x="best",ret="threshold",best.method="closest.topleft")
pred_BP1_train<-ifelse(pred_BP1_train<opt_BP1,0,1)
confusionMatrix(data=as.factor(pred_BP1_train),reference=as.factor(train$TenYearCHD))
#0.7794,0.8079,0.7510

pred_BP1_test<-predict(object=fit_BP1,newdata=test[,-15],type="response")
roc_BP1_test<-roc(test$TenYearCHD,pred_BP1_test)
plot(roc_BP1_test,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE) #0.731
pred_BP1_test<-ifelse(pred_BP1_test<opt_BP1,0,1)
confusionMatrix(data=as.factor(pred_BP1_test),reference=as.factor(test$TenYearCHD))
#0.7712,0.8269,0.4524

##2.PCA on BP
pc<-princomp(train[,c("sysBP","diaBP")])
summary(pc,loadings=T)

train$BP<-as.matrix(train[,c("sysBP","diaBP")])%*%pc$loadings[,1]
test$BP<-as.matrix(test[,c("sysBP","diaBP")])%*%pc$loadings[,1]

fit_BP2<-stepAIC(glm(TenYearCHD~.,data=train[,c(-10,-11)],family=binomial(link="logit")))
summary(fit_BP2)

pred_BP2_train<-predict(fit_BP2,newdata=train[,c(-10,-11,-15)],type="response",quiet=T)
roc_BP2_train<-roc(train$TenYearCHD,pred_BP2_train)
plot(roc_BP2_train,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE) #0.865
opt_BP2<-coords(roc=roc_BP2_train,x="best",ret="threshold",best.method="closest.topleft")
pred_BP2_train<-ifelse(pred_BP2_train<opt_BP2,0,1)
confusionMatrix(data=as.factor(pred_BP2_train),reference=as.factor(train$TenYearCHD))
#0.7835,0.8137,0.7534

pred_BP2_test<-predict(object=fit_BP2,newdata=test[,c(-10,-11,-15)],type="response")
roc_BP2_test<-roc(test$TenYearCHD,pred_BP2_test)
plot(roc_BP2_test,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE) #0.730
pred_BP2_test<-ifelse(pred_BP2_test<opt_BP2,0,1)
confusionMatrix(data=as.factor(pred_BP2_test),reference=as.factor(test$TenYearCHD))
#0.7748,0.8324,0.4444

##outcome:PCA on BP
summary(fit_BP2)

###BMI:discrtize

cut<-seq(from=24,to=30,by=0.1)
stat<-matrix(NA,ncol=2,nrow=length(cut))
stat[,1]<-cut
for(i in 1:length(cut)){
  train$BMI_dis<-1+(train$BMI>=cut[i])
  m<-chisq.test(table(train$BMI_dis,train$TenYearCHD))
  stat[i,2]<-m$statistic
}
stat<-as.data.frame(stat)
colnames(stat)<-c("cut","statistic")
ggplot(data=stat,aes(x=cut,y=statistic))+geom_line()+geom_point()+geom_point(data=stat[5,],col="red",size=3)

train$BMI_dis<-as.numeric((train$BMI>=24.4))
train$BMI_dis<-as.factor(train$BMI_dis)
fit_BMI<-glm(TenYearCHD~male+age+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+diabetes+totChol+heartRate+ 
            glucose+BP+BMI_dis,data=train,family=binomial(link="logit"))
summary(fit_BMI)

pred_BMI<-predict(fit_BMI,newdata=train,type="response",quiet=T)
roc_BMI<-roc(train$TenYearCHD,pred_BMI)
plot(roc_BMI,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE) #0.861
opt_BMI<-coords(roc=roc_BMI,x="best",ret="threshold",best.method="closest.topleft")
pred_BMI<-ifelse(pred_BMI<opt_BMI,0,1)
confusionMatrix(data=as.factor(pred_BMI),reference=as.factor(train$TenYearCHD))
#0.7835,0.8137,0.7534

test$BMI_dis<-as.factor(as.numeric((test$BMI>=24.4)))
pred_B<-predict(object=fit_BMI,newdata=test,type="response")
roc_B<-roc(test$TenYearCHD,pred_B)
plot(roc_B,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE) #0.730
pred_B<-ifelse(pred_B<opt_BMI,0,1)
confusionMatrix(data=as.factor(pred_B),reference=as.factor(test$TenYearCHD))
#0.7748,0.8324,0.4444


fit_LR_final<-stepAIC(fit_BMI)
summary(fit_LR_final)

pred_LR_train<-predict(object=fit_LR_final,newdata=train,type="response")
roc_LR_train<-roc(train$TenYearCHD,pred_LR_train)
plot(roc_LR_train,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE) #0.861
opt_LR<-coords(roc=roc_LR_train,x="best",ret="threshold",best.method="closest.topleft")
pred_LR_train<-ifelse(pred_LR_train<opt_LR,0,1)
confusionMatrix(data=as.factor(pred_LR_train),reference=as.factor(train$TenYearCHD))
#0.7782,0.804,0.7519

pred_LR_test<-predict(object=fit_LR_final,newdata=test,type="response")
roc_LR_test<-roc(test$TenYearCHD,pred_LR_test)
plot(roc_LR_test,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE) #0.731
pred_LR_test<-ifelse(pred_LR_test<opt_LR,0,1)
confusionMatrix(data=as.factor(pred_LR_test),reference=as.factor(test$TenYearCHD))
#0.7759,0.8310,0.4603




