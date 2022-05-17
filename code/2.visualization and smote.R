###data visualization and smote

library(ggplot2)
library(MASS)
library(pROC)
library(caret)
library(DMwR)

setwd("C:/Users/20833/Desktop/Êý¾ÝÍÚ¾ò/Project")
data<-read.csv("preprocess.csv",header=T)
any(is.na(data))

data<-data[,-4]

#####data visualization
 

##sysBP
ggplot(data=data,aes(x=as.factor(TenYearCHD),y=sysBP,fill=as.factor(TenYearCHD)))+geom_boxplot(varwidth=T)+xlab("CHD")

##cigs
ggplot(data=data,aes(x=as.factor(TenYearCHD),y=cigsPerDay,fill=as.factor(TenYearCHD)))+geom_boxplot(varwidth=T)+xlab("CHD")

##BMI
ggplot(data=data,aes(x=as.factor(TenYearCHD),y=age,fill=as.factor(TenYearCHD)))+geom_boxplot(varwidth=T)+xlab("CHD")


#####train set and test set
set.seed(2020)
ind<-sample(1:nrow(data),size=0.8*nrow(data))
test<-data[-ind,]
train<-data[ind,]

table(train$TenYearCHD)
table(test$TenYearCHD)


#####logistic regression without oversampling
for(i in c(1,3,5,6,7,8,15)){
  train[,i]<-as.factor(train[,i])
  test[,i]<-as.factor(test[,i])
}

#model
fit1<-glm(TenYearCHD~.,data=train,family=binomial(link="logit"))
fit1<-stepAIC(fit1)
summary(fit1)

#predict on train set
pred_fit1<-predict(fit1,data=train[,-15],type="response",quiet=T)
roc_train_fit1<-roc(train$TenYearCHD,pred_fit1)
plot(roc_train_fit1,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE)
opt<-coords(roc=roc_train_fit1,x="best",ret="threshold",best.method="closest.topleft")
pred_fit1<-ifelse(pred_fit1<opt,0,1)
confusionMatrix(data=as.factor(pred_fit1),reference=as.factor(train$TenYearCHD))

#predict on test set
pred_fit1_test<-predict(fit1,newdata=test[,-15],type="response",quiet=T)
pred_fit1_test<-ifelse(pred_fit1_test<opt,0,1)
roc_test_fit1<-roc(test$TenYearCHD,pred_fit1_test)
plot(roc_test_fit1,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue")
confusionMatrix(data=as.factor(pred_fit1_test),reference=as.factor(test$TenYearCHD))

######SMOTE
##find best coefficient
k<-1:8
perc.over<-seq(from=100,to=300,by=25)

iter<-matrix(NA,nrow=length(k),ncol=length(perc.over))

for(j in 1:length(perc.over)){
  for (i in 1:length(k)) {
    set.seed(2020)
    train_smote<-SMOTE(TenYearCHD~.,train,perc.over=perc.over[j],k=k[i],perc.under=100*(100+perc.over[j])/perc.over[j])
    fit_smote<-glm(formula=TenYearCHD~.,data=train_smote,family = binomial(link="logit"))
    pred_train_smote<-predict(object=fit_smote,newdata=train_smote,type="response")
    auc_train_smote<-auc(train_smote$TenYearCHD,pred_train_smote,quiet=T)
    iter[i,j]<-as.numeric(auc_train_smote)
  }
}
which(iter==iter[which.max(iter)],arr.ind=T)

set.seed(2020)
train_smote<-SMOTE(TenYearCHD~.,train,perc.over=300,k=1,perc.under=100*(100+300)/300)
fit_smote<-glm(formula=TenYearCHD~.,data=train_smote,family = binomial(link="logit"))
fit_smote<-stepAIC(fit_smote)
summary(fit_smote)

pred_train_smote<-predict(object=fit_smote,newdata=train_smote,type="response")
roc_train_smote<-roc(train_smote$TenYearCHD,pred_train_smote)
plot(roc_train_smote,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE)
opt_smote<-coords(roc=roc_train_smote,x="best",ret="threshold",best.method="closest.topleft")
pred_train_smote<-ifelse(pred_train_smote<opt_smote,0,1)
confusionMatrix(data=as.factor(pred_train_smote),reference=as.factor(train_smote$TenYearCHD))
pred_test_smote<-predict(object=fit_smote,newdata=test,type="response")
roc_test_smote<-roc(test$TenYearCHD,pred_test_smote)
plot(roc_test_smote,print.auc=T,auc.polygon=TRUE,max.auc.polygon=TRUE,grid=c(0.1,0.2),
     auc.polygon.col="skyblue", print.thres=TRUE)
pred_test_smote<-ifelse(pred_test_smote<opt_smote,0,1)
confusionMatrix(data=as.factor(pred_test_smote),reference=as.factor(test$TenYearCHD))


write.csv(train_smote,"train with smote.csv",row.names=F)
write.csv(test,"test.csv",row.names=F)

