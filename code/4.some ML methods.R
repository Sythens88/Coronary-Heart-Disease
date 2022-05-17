library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(maptree)
library(randomForest)


setwd("C:/Users/20833/Desktop/Êý¾ÝÍÚ¾ò/Project")
train<-read.csv("train with smote.csv",header=T)
test<-read.csv("test.csv",header=T)

for(i in c(1,3,5,6,7,8,15)){
  train[,i]<-as.factor(train[,i])
  test[,i]<-as.factor(test[,i])
}

#####1.cart desicion tree
set.seed(2020)
fit_cdt<-rpart(TenYearCHD~.,data=train)
windows()
rpart.plot(fit_cdt,type=4,fallen.leaves=T)

pred_cdt_train<-predict(fit_cdt,train,type="class")
confusionMatrix(pred_cdt_train, train$TenYearCHD) #0.8041,0.8436,0.7645
pred_cdt_test<-predict(fit_cdt,test,type="class") 
confusionMatrix(pred_cdt_test, test$TenYearCHD) #0.7512,0.8199,0.3571
pred_cdt_p<-predict(fit_cdt,train,type="prob")[,2]
roc_cdt<-roc(train$TenYearCHD,pred_cdt_p,quiet=T)
plot(roc_cdt, print.auc=TRUE, auc.polygon=TRUE,grid=c(0.1, 0.2),max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
pred_cdt_p<-predict(fit_cdt,test,type="prob")[,2]
roc_cdt<-roc(test$TenYearCHD,pred_cdt_p,quiet=T)
plot(roc_cdt, print.auc=TRUE, auc.polygon=TRUE,grid=c(0.1, 0.2),max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE) #0.641

##prune
plotcp(fit_cdt,minline=TRUE,lty=3,col=1,upper=c("size","splits","none"))
fit_cdt_prune<-prune(fit_cdt,0.017)
windows()
#rpart.plot(fit_cdt_prune,type=4,branch=1,fallen.leaves=T)
draw.tree(fit_cdt_prune)

pred_cdt_prune_train<-predict(fit_cdt_prune,train,type="class")
confusionMatrix(pred_cdt_prune_train, train$TenYearCHD) #0.7638,0.6602,0.8673
pred_cdt_prune_test<-predict(fit_cdt_prune,test,type="class") 
confusionMatrix(pred_cdt_prune_test, test$TenYearCHD) #0.645,0.6524,0.6032
pred_cdt_prune_p<-predict(fit_cdt_prune,train,type="prob")[,2]
roc_cdt_prune<-roc(train$TenYearCHD,pred_cdt_prune_p,quiet=T)
plot(roc_cdt_prune, print.auc=TRUE, auc.polygon=TRUE,grid=c(0.1, 0.2),max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE) #0.625
pred_cdt_prune_p<-predict(fit_cdt_prune,test,type="prob")[,2]
roc_cdt_prune<-roc(test$TenYearCHD,pred_cdt_prune_p,quiet=T)
plot(roc_cdt_prune, print.auc=TRUE, auc.polygon=TRUE,grid=c(0.1, 0.2),max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE) #0.625


#####2.random forest
set.seed(2020)
fit_rf<-randomForest(TenYearCHD~.,data=train,proximity=T,importance=T)
plot(fit_rf, main="Random Forest (Error Rate vs. Number of Trees)")

pred_rf_train<-predict(fit_rf,train)
confusionMatrix(pred_rf_train,train$TenYearCHD) #1,1,1
pred_rf_test<-predict(fit_rf,test)
confusionMatrix(pred_rf_test,test$TenYearCHD) #0.7512,0.8255,0.3254

pred_rf_test_p<-predict(fit_rf,test,type = "prob")[,1]
roc_rf<-roc(test$TenYearCHD,pred_rf_test_p,quiet = T)
plot(roc_rf, print.auc=TRUE, auc.polygon=TRUE,grid=c(0.1, 0.2),max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE) #0.694

##mtry&ntree
rf_train<-train
set.seed(2020)
rf_train$cv<-ceiling(runif(4144,0,5))

mtry<-3:7
ntree<-seq(from=200,to=600,by=50)

iter<-matrix(0,nrow=length(mtry),ncol=length(ntree))
for(i in 1:length(mtry)){
  for (j in 1:length(ntree)){
    for(k in 1:5){
      set.seed(2020)
      fit_rf2<-randomForest(TenYearCHD~.,data=rf_train[rf_train$cv!=k,-16],mtry=mtry[i],ntree=ntree[j],proximity=T,importance=T)
      pred_rf2<-predict(fit_rf2,rf_train[rf_train$cv==k,])
      A<-as.matrix(table(pred_rf2,rf_train[rf_train$cv==k,"TenYearCHD"]))
      iter[i,j]<-iter[i,j]+sum(diag(A))/sum(A) 
    }
  iter[i,j]<-iter[i,j]/5
  }
}
iter
which(iter==iter[which.max(iter)],arr.ind=T)


set.seed(2020)
fit_rf2<-randomForest(TenYearCHD~.,data=train,proximity=T,importance=T,mtry=3,ntree=550)
plot(fit_rf2, main="Random Forest (Error Rate vs. Number of Trees)")

pred_rf2_train<-predict(fit_rf2,train)
confusionMatrix(pred_rf2_train,train$TenYearCHD) #1,1,1
pred_rf2_test<-predict(fit_rf2,test)
confusionMatrix(pred_rf2_test,test$TenYearCHD) #0.7606,0.8352,0.3333

pred_rf2_train_p<-predict(fit_rf2,train,type = "prob")[,1]
roc_rf2<-roc(train$TenYearCHD,pred_rf2_train_p,quiet = T)
plot(roc_rf2, print.auc=TRUE, auc.polygon=TRUE,grid=c(0.1, 0.2),max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE) #0.687

pred_rf2_test_p<-predict(fit_rf2,test,type = "prob")[,1]
roc_rf2<-roc(test$TenYearCHD,pred_rf2_test_p,quiet = T)
plot(roc_rf2, print.auc=TRUE, auc.polygon=TRUE,grid=c(0.1, 0.2),max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE) #0.687

fit_rf2$importance
varImpPlot(fit_rf2,main="importance factor")






