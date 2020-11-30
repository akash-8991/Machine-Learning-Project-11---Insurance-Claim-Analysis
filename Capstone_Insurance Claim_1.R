setwd("C:/Users/Akash/Desktop/Study/PGP BABI/Capstone Project/Insurance Claims")
library(e1071)
library(rpart)
library(readxl)
library(xgboost)
library(adabag)
library(NbClust)
library(tidyr)
library(dplyr)
library(ROSE)
library(ggplot2)
library(imbalance)
library(lattice)
library(mice)
library(corrplot)
library(NbClust)
library(tidyverse)
library(iterators)
library(caret)
library(psych)
library(caTools)
library(car)
library(rpart)
library(randomForest)
library(data.table)
library(scales)
library(rpart.plot)
library(ROCR)
library(ineq)
library(InformationValue)
library(cluster)
library(clValid)
library(factoextra)
library(DMwR)

claimdata=read_excel("Insurance Claims Data_Use1.xlsx")
usedata=claimdata
str(usedata)
summary(usedata)

#Coverting data type of variabels to correct format
usedata$Txt_Policy_Year=as.factor(usedata$Txt_Policy_Year)
usedata$Boo_Endorsement=as.factor(usedata$Boo_Endorsement)
usedata$Txt_Location_RTA_Policy_Penetration=as.factor(usedata$Txt_Location_RTA_Policy_Penetration)
usedata$Txt_Policy_Code=as.factor(usedata$Txt_Policy_Code)
usedata$Txt_Class_Code=as.factor(usedata$Txt_Class_Code)
usedata$Txt_Zone_Code=as.factor(usedata$Txt_Zone_Code)
usedata$Num_Vehicle_Age=as.numeric(usedata$Num_Vehicle_Age)
usedata$Txt_CC_PCC_GVW_Code=as.factor(usedata$Txt_CC_PCC_GVW_Code)
usedata$Txt_Colour_Vehicle=as.factor(usedata$Txt_Colour_Vehicle)
usedata$Txt_Colour_Vehicle=as.numeric(usedata$Txt_Colour_Vehicle)
usedata$Txt_Permit_Code=as.factor(usedata$Txt_Permit_Code)
usedata$Txt_Nature_Goods_Code=as.factor(usedata$Txt_Nature_Goods_Code)
usedata$Txt_Road_Type_Code=as.factor(usedata$Txt_Road_Type_Code)
usedata$Txt_Vehicle_Driven_By_Code=as.factor(usedata$Txt_Vehicle_Driven_By_Code)
usedata$Txt_Driver_Exp_Code=as.factor(usedata$Txt_Driver_Exp_Code)
usedata$Txt_Claims_History_Code=as.factor(usedata$Txt_Claims_History_Code)
usedata$Txt_Driver_Qualification_Code=as.factor(usedata$Txt_Driver_Qualification_Code)
usedata$Txt_Incurred_Claims_Code=as.numeric(usedata$Txt_Incurred_Claims_Code)
usedata$Boo_TPPD_Statutory_Cover_only=as.factor(usedata$Boo_TPPD_Statutory_Cover_only)
usedata$Txt_Claim_Year=as.factor(usedata$Txt_Claim_Year)
usedata$Txt_Place_Accident_Policy_Penetration=as.factor(usedata$Txt_Place_Accident_Policy_Penetration)
usedata$Txt_TAC_NOL_Code=as.factor(usedata$Txt_TAC_NOL_Code)
usedata$Boo_OD_Total_Loss=as.factor(usedata$Boo_OD_Total_Loss)
usedata$DRV_CLAIM_STATUS=as.factor(usedata$DRV_CLAIM_STATUS)
usedata$Boo_AntiTheft=as.factor(usedata$Boo_AntiTheft)
usedata$Boo_NCB=as.factor(usedata$Boo_NCB)
str(usedata)
usedata1=usedata[,-1]
str(usedata1)
summary(usedata1)

#Outlier Treatment
boxplot(usedata1$Num_IDV)
boxplot(usedata1$DRV_CLAIM_AMT)
boxplot(usedata1$Num_Net_OD_Premium)
for (i in 1:75200) {
  usedata1$Num_IDV[i]=ifelse(usedata1$Num_IDV[i]>(quantile(usedata1$Num_IDV,0.75)+(1.5*(quantile(usedata1$Num_IDV,0.75)-quantile(usedata1$Num_IDV,0.25)))),
                             quantile(usedata1$Num_IDV, 0.75)+(1.5*(quantile(usedata1$Num_IDV,0.75)-quantile(usedata1$Num_IDV,0.25))), usedata1$Num_IDV[i])
  usedata1$DRV_CLAIM_AMT[i]=ifelse(usedata1$DRV_CLAIM_AMT[i]>(quantile(usedata1$DRV_CLAIM_AMT,0.75)+(1.5*(quantile(usedata1$DRV_CLAIM_AMT,0.75)-quantile(usedata1$DRV_CLAIM_AMT,0.25)))),
                                   quantile(usedata1$DRV_CLAIM_AMT,0.75)+(1.5*(quantile(usedata1$DRV_CLAIM_AMT,0.75)-quantile(usedata1$DRV_CLAIM_AMT,0.25))), usedata1$DRV_CLAIM_AMT[i])
  usedata1$Num_Net_OD_Premium[i]=ifelse(usedata1$Num_Net_OD_Premium[i]>(quantile(usedata1$Num_Net_OD_Premium,0.75)+(1.5*(quantile(usedata1$Num_Net_OD_Premium,0.75)-quantile(usedata1$Num_Net_OD_Premium,0.25)))),
                                        quantile(usedata1$Num_Net_OD_Premium,0.75)+(1.5*(quantile(usedata1$Num_Net_OD_Premium,0.75)-quantile(usedata1$Num_Net_OD_Premium,0.25))), usedata1$Num_Net_OD_Premium[i])
}
summary(usedata1)

#Missing Value Check
sum(is.na(usedata1)) #We remove the disbursement date as it will have 100% correlation with Claim Status
usedata1=usedata1[,-c(29:31)]
str(usedata1)
#Univariate Analysis
hist(usedata1$Num_Vehicle_Age,main="Vehicle Age",xlab="Vehicle Age") #Right Skewed
hist(usedata1$Num_IDV,main="Sum Insured",xlab="Sum Insured") #Right Skewed
hist(usedata1$Txt_Incurred_Claims_Code,main="Claim Exp.of Previous Years",xlab="Claim Exp.of Previous Years")
hist(usedata1$Date_Accident_Loss_Day,main="Day of Accident/Loss",xlab="Day of Accident/Loss") #Normal
hist(usedata1$Date_Accident_Loss_Month,main="Month of Accident/Loss",xlab="Month of Accident/Loss") #Right Skewed
hist(usedata1$Date_Accident_Loss_Year,main="Year of Accident/Loss",xlab="Year of Accident/Loss") #Left Skewed
hist(usedata1$Date_Claim_Intimation_Day,main="Day of Claim Initmation",xlab="Day of Claim Initmation") #Normal
hist(usedata1$Date_Claim_Intimation_Month,main="Month of Claim Initmation",xlab="Month of Claim Initmation") #Right Skewed
hist(usedata1$Date_Claim_Intimation_Year,main="Year of Claim Initmation",xlab="Year of Claim Initmation") #Left Skewed
hist(usedata1$DRV_CLAIM_AMT,main="Claim Amount",xlab="Claim Amount") #Right Skewed
hist(usedata1$Num_Net_OD_Premium,main="Net OD Premium",xlab="Net OD Premium") #Normal

table(usedata1$Txt_Policy_Year)
table(usedata1$Boo_Endorsement)
table(usedata1$Txt_Location_RTA_Policy_Penetration)
table(usedata1$Txt_Policy_Code)
table(usedata1$Txt_Class_Code)
table(usedata1$Txt_Zone_Code)
table(usedata1$Txt_CC_PCC_GVW_Code)
table(usedata1$Txt_Permit_Code)
table(usedata1$Txt_Nature_Goods_Code)
table(usedata1$Txt_Road_Type_Code)
table(usedata1$Txt_Vehicle_Driven_By_Code)
table(usedata1$Txt_Driver_Exp_Code)
table(usedata1$Txt_Claims_History_Code)
table(usedata1$Txt_Driver_Qualification_Code)
table(usedata1$Boo_TPPD_Statutory_Cover_only)
table(usedata1$Txt_Claim_Year)
table(usedata1$Txt_Place_Accident_Policy_Penetration)
table(usedata1$Txt_TAC_NOL_Code)
table(usedata1$Boo_OD_Total_Loss)
table(usedata1$DRV_CLAIM_STATUS)
table(usedata1$Boo_AntiTheft)
table(usedata1$Boo_NCB)

#Collinearity Analysis
finalusedata=usedata1
usedata2 = usedata1 %>% select_if(is.numeric)
cor(usedata2, use = "pairwise.complete.obs")
  #Variables to remove,
    #Date_Accident_Loss_Month
    #Date_Accident_Loss_Year
finalusedata=finalusedata[,-c(22,23)]
usedata2_1 = finalusedata %>% select_if(is.numeric)
cor(usedata2_1, use = "pairwise.complete.obs") #Removing the above mentioned 5 independent variables
                                               #we have completely removed multicollinearity. Any variable
                                               #with more that 75% collinearity has been removed
corrplot(cor(usedata2_1, use = "pairwise.complete.obs"))

sum(is.na(finalusedata))
finalusedata1=finalusedata
finalusedata1$Txt_Policy_Year=as.numeric(finalusedata1$Txt_Policy_Year)
finalusedata1$Boo_Endorsement=as.numeric(finalusedata1$Boo_Endorsement)
finalusedata1$Txt_Location_RTA_Policy_Penetration=as.numeric(finalusedata1$Txt_Location_RTA_Policy_Penetration)
finalusedata1$Txt_Policy_Code=as.numeric(finalusedata1$Txt_Policy_Code)
finalusedata1$Txt_Class_Code=as.numeric(finalusedata1$Txt_Class_Code)
finalusedata1$Txt_Zone_Code=as.numeric(finalusedata1$Txt_Zone_Code)
finalusedata1$Txt_CC_PCC_GVW_Code=as.numeric(finalusedata1$Txt_CC_PCC_GVW_Code)
finalusedata1$Txt_Permit_Code=as.numeric(finalusedata1$Txt_Permit_Code)
finalusedata1$Txt_Nature_Goods_Code=as.numeric(finalusedata1$Txt_Nature_Goods_Code)
finalusedata1$Txt_Road_Type_Code=as.numeric(finalusedata1$Txt_Road_Type_Code)
finalusedata1$Txt_Vehicle_Driven_By_Code=as.numeric(finalusedata1$Txt_Vehicle_Driven_By_Code)
finalusedata1$Txt_Driver_Exp_Code=as.numeric(finalusedata1$Txt_Driver_Exp_Code)
finalusedata1$Txt_Claims_History_Code=as.numeric(finalusedata1$Txt_Claims_History_Code)
finalusedata1$Txt_Driver_Qualification_Code=as.numeric(finalusedata1$Txt_Driver_Qualification_Code)
finalusedata1$Boo_TPPD_Statutory_Cover_only=as.numeric(finalusedata1$Boo_TPPD_Statutory_Cover_only)
finalusedata1$Txt_Claim_Year=as.numeric(finalusedata1$Txt_Claim_Year)
finalusedata1$Txt_Place_Accident_Policy_Penetration=as.numeric(finalusedata1$Txt_Place_Accident_Policy_Penetration)
finalusedata1$Txt_TAC_NOL_Code=as.numeric(finalusedata1$Txt_TAC_NOL_Code)
finalusedata1$Boo_OD_Total_Loss=as.numeric(finalusedata1$Boo_OD_Total_Loss)
finalusedata1$Boo_AntiTheft=as.numeric(finalusedata1$Boo_AntiTheft)
finalusedata1$Boo_NCB=as.numeric(finalusedata1$Boo_NCB)


vifmatrix=vif(glm(finalusedata1$DRV_CLAIM_STATUS~.,data=finalusedata1, family = binomial(link="logit")))
vifmatrix

# treatment for multicollinearity using PCA

components=principal(finalusedata1[,-29],nfactors=16,rotate="varimax")
components #extracting factors without rotation
fa.diagram(components) #the component analysis plot clearly does not
dataset1=cbind(finalusedata1[,29],components$scores)
head(dataset1)
#colnames(dataset1)[9]=c("MonthlyPlan")
#head(dataset1)

#Bivariate Analysis
str(finalusedata)
table(finalusedata$Txt_Policy_Year, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Boo_Endorsement, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Location_RTA_Policy_Penetration, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Policy_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Class_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Zone_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_CC_PCC_GVW_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Permit_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Nature_Goods_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Road_Type_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Vehicle_Driven_By_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Driver_Exp_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Claims_History_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Driver_Qualification_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Boo_TPPD_Statutory_Cover_only, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Claim_Year, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_Place_Accident_Policy_Penetration, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Txt_TAC_NOL_Code, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Boo_OD_Total_Loss, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Boo_AntiTheft, finalusedata$DRV_CLAIM_STATUS)
table(finalusedata$Boo_NCB, finalusedata$DRV_CLAIM_STATUS)
c1=aggregate(finalusedata$Num_IDV, 
          by=list(Category=finalusedata$DRV_CLAIM_STATUS),
          FUN = sum)
c2=aggregate(finalusedata$DRV_CLAIM_AMT, 
          by=list(Category=finalusedata$DRV_CLAIM_STATUS),
          FUN = sum)
c3=aggregate(finalusedata$Num_Net_OD_Premium, 
          by=list(Category=finalusedata$DRV_CLAIM_STATUS),
          FUN = sum)
cbind(table(finalusedata$DRV_CLAIM_STATUS),c1[,-1]) #SI
cbind(table(finalusedata$DRV_CLAIM_STATUS),c2[,-1]) #Claim Amount
cbind(table(finalusedata$DRV_CLAIM_STATUS),c3[,-1]) #Net OD Premium 
                                                    #Service Tax

# K-means Clustering to identify the characteristics of the group with maximum rejects

modeldata = finalusedata
clusteringdata = finalusedata
clusteringdata1 = finalusedata

str(clusteringdata)
clusteringdata$Txt_Policy_Year=as.numeric(clusteringdata$Txt_Policy_Year)
clusteringdata$Boo_Endorsement=as.numeric(clusteringdata$Boo_Endorsement)
clusteringdata$Txt_Location_RTA_Policy_Penetration=as.numeric(clusteringdata$Txt_Location_RTA_Policy_Penetration)
clusteringdata$Txt_Policy_Code=as.numeric(clusteringdata$Txt_Policy_Code)
clusteringdata$Txt_Class_Code=as.numeric(clusteringdata$Txt_Class_Code)
clusteringdata$Txt_Zone_Code=as.numeric(clusteringdata$Txt_Zone_Code)
clusteringdata$Txt_CC_PCC_GVW_Code=as.numeric(clusteringdata$Txt_CC_PCC_GVW_Code)
clusteringdata$Txt_Permit_Code=as.numeric(clusteringdata$Txt_Permit_Code)
clusteringdata$Txt_Nature_Goods_Code=as.numeric(clusteringdata$Txt_Nature_Goods_Code)
clusteringdata$Txt_Road_Type_Code=as.numeric(clusteringdata$Txt_Road_Type_Code)
clusteringdata$Txt_Vehicle_Driven_By_Code=as.numeric(clusteringdata$Txt_Vehicle_Driven_By_Code)
clusteringdata$Txt_Driver_Exp_Code=as.numeric(clusteringdata$Txt_Driver_Exp_Code)
clusteringdata$Txt_Claims_History_Code=as.numeric(clusteringdata$Txt_Claims_History_Code)
clusteringdata$Txt_Driver_Qualification_Code=as.numeric(clusteringdata$Txt_Driver_Qualification_Code)
clusteringdata$Boo_TPPD_Statutory_Cover_only=as.numeric(clusteringdata$Boo_TPPD_Statutory_Cover_only)
clusteringdata$Txt_Claim_Year=as.numeric(clusteringdata$Txt_Claim_Year)
clusteringdata$Txt_Place_Accident_Policy_Penetration=as.numeric(clusteringdata$Txt_Place_Accident_Policy_Penetration)
clusteringdata$Txt_TAC_NOL_Code=as.numeric(clusteringdata$Txt_TAC_NOL_Code)
clusteringdata$Boo_OD_Total_Loss=as.numeric(clusteringdata$Boo_OD_Total_Loss)
clusteringdata$Boo_AntiTheft=as.numeric(clusteringdata$Boo_AntiTheft)
clusteringdata$Boo_NCB=as.numeric(clusteringdata$Boo_NCB)

clusteringdata1=clusteringdata[,-29]
clusteringdata1=cbind(clusteringdata1,clusteringdata[,29])

str(clusteringdata1)
sum(is.na(clusteringdata1))
clusteringdata2=clusteringdata1
clusteringdata2$DRV_CLAIM_STATUS=as.numeric(clusteringdata2$DRV_CLAIM_STATUS)

wss=(nrow(clusteringdata2)-1)*sum(apply(clusteringdata2,2,var))
  for (j in 2:20) {
    wss[j]=sum(kmeans(clusteringdata2,centers = j)$withinss)
  }
plot(1:20, wss, type="b",xlab="No. of Groups", ylab="Within SS of group")

cluster_fit=kmeans(clusteringdata2,3)
clusteringdata2=data.frame(clusteringdata2,cluster_fit$cluster)

table(clusteringdata2$DRV_CLAIM_STATUS,clusteringdata2$cluster_fit.cluster)
fviz_cluster(cluster_fit,clusteringdata2[,-c(32,33)],ellipse.type = "norm")

getmode = function(v){
  levels(v)[which.max(table(v))]
}
my_summary=function(x,id,...){
  if (is.numeric(x)) {
    return(tapply(x,id,mean))
  }
  if(is.factor(x)){
    return(tapply(x,id,getmode))
  }
}
data.frame(lapply(clusteringdata2,my_summary, 
                  id=clusteringdata2$cluster_fit.cluster))

#Model Building - Logistic Regression, Random Forest, NB, KNN, Bagging and Boosting

seed=1234
set.seed(seed)
sample=sample.split(dataset1$DRV_CLAIM_STATUS,SplitRatio = 0.7)
train=subset(dataset1,sample == TRUE)
test=subset(dataset1,sample == FALSE)
train1=train
str(train1)

#Without SMOTE

train_smote=train1

#Logistic Regression
t1=train_smote
t2=test
log_Reg_Model <- glm(t1$DRV_CLAIM_STATUS ~., data = t1, family = "binomial")
log_Reg_Model
summary(log_Reg_Model)
anova(log_Reg_Model, test="Chisq")

ctable = coef(summary(log_Reg_Model))
ctable
t1$Pred <- predict(log_Reg_Model, newdata = t1, type = "response")

train_tab <- table(t1$DRV_CLAIM_STATUS, t1$Pred>0.9)
train_tab
sum(diag(train_tab))/sum(train_tab)
t1_sensitivity=train_tab[1,1]/sum(train_tab[1,])
t1_sensitivity
t1_specificity=train_tab[2,2]/sum(train_tab[2,])
t1_specificity

t2$Pred <- predict(log_Reg_Model, newdata = t2, type = "response")
test_tab <- table(t2$DRV_CLAIM_STATUS, t2$Pred>0.9)
test_tab
sum(diag(test_tab))/sum(test_tab)
t2_sensitivity=test_tab[1,1]/sum(test_tab[1,])
t2_sensitivity
t2_specificity=test_tab[2,2]/sum(test_tab[2,])
t2_specificity

train_LR_predobj <- prediction(t1$Pred, t1$DRV_CLAIM_STATUS)
train_LR_perf <- performance(train_LR_predobj, "tpr", "fpr")
plot(train_LR_perf)
train_LR_KS <- max(attr(train_LR_perf, 'y.values')[[1]]-attr(train_LR_perf, 'x.values')[[1]])
train_LR_auc <- performance(train_LR_predobj,"auc"); 
train_LR_auc <- as.numeric(train_LR_auc@y.values)
train_LR_gini = ineq(t1$Pred, type="Gini")
train_LR_auc
train_LR_KS
train_LR_gini

test_LR_predobj <- prediction(t2$Pred, t2$DRV_CLAIM_STATUS)
test_LR_perf <- performance(test_LR_predobj, "tpr", "fpr")
plot(test_LR_perf)
test_LR_KS <- max(attr(test_LR_perf, 'y.values')[[1]]-attr(test_LR_perf, 'x.values')[[1]])
test_LR_auc <- performance(test_LR_predobj,"auc"); 
test_LR_auc <- as.numeric(test_LR_auc@y.values)
test_LR_gini = ineq(t2$Pred, type="Gini")
test_LR_auc
test_LR_KS
test_LR_gini

#Naive Bayes
t5=train_smote
t6=test
NB = naivebayes::naive_bayes(t5$DRV_CLAIM_STATUS ~., data = t5, laplace = 1)
NB
t5$pred = predict(NB, t5, type = "class")
train_tab.NB = table(t5$DRV_CLAIM_STATUS, t5$pred)
train_tab.NB
sum(diag(train_tab.NB))/sum(train_tab.NB)
t5_sensitivity=train_tab.NB[1,1]/sum(train_tab.NB[1,])
t5_sensitivity
t5_specificity=train_tab.NB[2,2]/sum(train_tab.NB[2,])
t5_specificity

t6$pred = predict(NB, t6, type = "class")
test_tab.NB = table(t6$DRV_CLAIM_STATUS, t6$pred)
test_tab.NB
sum(diag(test_tab.NB))/sum(test_tab.NB)
t6_sensitivity=test_tab.NB[1,1]/sum(test_tab.NB[1,])
t6_sensitivity
t6_specificity=test_tab.NB[2,2]/sum(test_tab.NB[2,])
t6_specificity

train_NB_predobj <- prediction(as.numeric(t5$pred), as.numeric(t5$DRV_CLAIM_STATUS))
train_NB_perf <- performance(train_NB_predobj, "tpr", "fpr")
plot(train_NB_perf)
train_NB_KS <- max(attr(train_NB_perf, 'y.values')[[1]]-attr(train_NB_perf, 'x.values')[[1]])
train_NB_auc <- performance(train_NB_predobj,"auc"); 
train_NB_auc <- as.numeric(train_NB_auc@y.values)
train_NB_gini = ineq(t5$pred, type="Gini")
train_NB_auc
train_NB_KS
train_NB_gini

test_NB_predobj <- prediction(as.numeric(t6$pred), as.numeric(t6$DRV_CLAIM_STATUS))
test_NB_perf <- performance(test_NB_predobj, "tpr", "fpr")
plot(test_NB_perf)
test_NB_KS <- max(attr(test_NB_perf, 'y.values')[[1]]-attr(test_NB_perf, 'x.values')[[1]])
test_NB_auc <- performance(test_NB_predobj,"auc"); 
test_NB_auc <- as.numeric(test_NB_auc@y.values)
test_NB_gini = ineq(t6$pred, type="Gini")
test_NB_auc
test_NB_KS
test_NB_gini

#Random Forest
train2=train_smote
test2=test
rndForest1=randomForest(train2$DRV_CLAIM_STATUS~.,data=train2,nTree=25,mtry=6,nodesize=1,importance=TRUE)
print(rndForest1)
print(rndForest1$err.rate)
plot(rndForest1)
importance(rndForest1)
rndForest2=tuneRF(x=train2[,-1],y=train2$DRV_CLAIM_STATUS,mtryStart=6,step=1.5,ntreeTry=25,
                  improve = 0.001,nodesize=1,trace=TRUE,plot=TRUE,dobest=TRUE,importance=TRUE)

TrndForest=randomForest(train2$DRV_CLAIM_STATUS~.,data=train2,nTree=25,mtry=9,nodesize=1,importance=TRUE)
print(TrndForest)
# measuring model performance of random forest model 
train2$prediction=predict(TrndForest,train2,type="class")
train2$prob=predict(TrndForest,train2,type="prob")
tbl_train2=table(train2$DRV_CLAIM_STATUS,train2$prediction)
print(tbl_train2)
print((tbl_train2[1,1]+tbl_train2[2,2])/sum(tbl_train2))

decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}      #creating decile function

train2$deciles <- decile(train2$prob[,2])
train2_DT = data.table(train2)
rank2 <- train2_DT[, list(cnt=length(train2$DRV_CLAIM_STATUS),
                          cnt_resp=sum(DRV_CLAIM_STATUS == "CLOSED"),
                          cnt_non_resp = sum(DRV_CLAIM_STATUS == "REJECTED")),
                   by=deciles][order(-deciles)]
rank2
rank2$rrate <- round(rank2$cnt_resp / rank2$cnt,4)*100 #in %
rank2$cum_resp <- cumsum(rank2$cnt_resp)
rank2$cum_non_resp <- cumsum(rank2$cnt_non_resp)
rank2$cum_rel_resp <- round(rank2$cum_resp / sum(rank2$cnt_resp),4)*100 #in %
rank2$cum_rel_non_resp <- round(rank2$cum_non_resp / sum(rank2$cnt_non_resp),4)*100 #in %
rank2$ks <- abs(rank2$cum_rel_resp - rank2$cum_rel_non_resp)
predobj1 <- prediction(train2$prob[,2], train2$DRV_CLAIM_STATUS)
perf1 <- performance(predobj1, "tpr", "fpr")
plot(perf1)
KS1 <- max(attr(perf1, 'y.values')[[1]]-attr(perf1, 'x.values')[[1]])
auc1 <- performance(predobj1,"auc"); 
auc1 <- as.numeric(auc1@y.values)
gini1 = ineq(train2$prob[,2], type="Gini")
auc1
KS1
gini1
#test the model on Test data
test2$prediction=predict(TrndForest,test2,type="class")
test2$prob=predict(TrndForest,test2,type="prob")
tbl_test2=table(test2$DRV_CLAIM_STATUS,test2$prediction) #confusion matrix
print(tbl_test2)
print((tbl_test2[1,1]+tbl_test2[2,2])/sum(tbl_test2))
test2$deciles <- decile(test2$prob[,2])
test2_DT = data.table(test2)
trank2 <- test2_DT[, list(cnt=length(test2$DRV_CLAIM_STATUS),
                          cnt_resp=sum(test2$DRV_CLAIM_STATUS == "CLOSED"),
                          cnt_non_resp = sum(test2$DRV_CLAIM_STATUS == "REJECTED")),
                   by=deciles][order(-deciles)]
trank2
trank2$rrate <- round(trank2$cnt_resp / trank2$cnt,4)*100 #in %
trank2$cum_resp <- cumsum(trank2$cnt_resp)
trank2$cum_non_resp <- cumsum(trank2$cnt_non_resp)
trank2$cum_rel_resp <- round(trank2$cum_resp / sum(trank2$cnt_resp),4)*100 #in %
trank2$cum_rel_non_resp <- round(trank2$cum_non_resp / sum(trank2$cnt_non_resp),4)*100 #in %
trank2$ks <- abs(trank2$cum_rel_resp - trank2$cum_rel_non_resp)
tpredobj1 <- prediction(test2$prob[,2], test2$DRV_CLAIM_STATUS)
tperf1 <- performance(tpredobj1, "tpr", "fpr")
plot(tperf1)
tKS1 <- max(attr(tperf1, 'y.values')[[1]]-attr(tperf1, 'x.values')[[1]])
tauc1 <- performance(tpredobj1,"auc"); 
tauc1 <- as.numeric(tauc1@y.values)
tgini1 = ineq(test2$prob[,2], type="Gini")
tauc1
tKS1
tgini1

#Bagging
library(ipred)
t11=train_smote
t12=test
BAGModel <- bagging(t11$DRV_CLAIM_STATUS~., data = t11,
                    control = rpart.control(maxdepth = 5, minsplit = 5))
BAGModel
summary(BAGModel)
t11$pred <- predict(BAGModel, t11)
trainBAG <- table(t11$DRV_CLAIM_STATUS, t11$pred)
trainBAG
sum(diag(trainBAG))/sum(trainBAG)
t11_sensitivity=trainBAG[1,1]/sum(trainBAG[1,])
t11_sensitivity
t11_specificity=trainBAG[2,2]/sum(trainBAG[2,])
t11_specificity

t12$pred <- predict(BAGModel, t12)
testBAG <- table(t12$DRV_CLAIM_STATUS, t12$pred)
testBAG
sum(diag(testBAG))/sum(testBAG)
t12_sensitivity=testBAG[1,1]/sum(testBAG[1,])
t12_sensitivity
t12_specificity=testBAG[2,2]/sum(testBAG[2,])
t12_specificity

train_bag_predobj <- prediction(as.numeric(t11$pred), as.numeric(t11$DRV_CLAIM_STATUS))
train_bag_perf <- performance(train_bag_predobj, "tpr", "fpr")
plot(train_bag_perf)
train_bag_KS <- max(attr(train_bag_perf, 'y.values')[[1]]-attr(train_bag_perf, 'x.values')[[1]])
train_bag_auc <- performance(train_bag_predobj,"auc"); 
train_bag_auc <- as.numeric(train_bag_auc@y.values)
train_bag_gini = ineq(t11$pred, type="Gini")
train_bag_auc
train_bag_KS
train_bag_gini

test_bag_predobj <- prediction(as.numeric(t12$pred),as.numeric(t12$DRV_CLAIM_STATUS))
test_bag_perf <- performance(train_bag_predobj, "tpr", "fpr")
plot(test_bag_perf)
test_bag_KS <- max(attr(test_bag_perf, 'y.values')[[1]]-attr(test_bag_perf, 'x.values')[[1]])
test_bag_auc <- performance(test_bag_predobj,"auc"); 
test_bag_auc <- as.numeric(test_bag_auc@y.values)
test_bag_gini = ineq(t12$pred, type="Gini")
test_bag_auc
test_bag_KS
test_bag_gini

#With Oversampling
#Because the data is unbalanced, first we need to smote the data

train3=train
str(train3)
summary(train3)

train_smote2=ovun.sample(DRV_CLAIM_STATUS~.,train3,method="over",N=99854)
train_smote1=train_smote2$data
table(train_smote1$DRV_CLAIM_STATUS)

#Logistic Regression
t1=train_smote1
t2=test
log_Reg_Model <- glm(t1$DRV_CLAIM_STATUS ~., data = t1, family = "binomial")
log_Reg_Model
summary(log_Reg_Model)
anova(log_Reg_Model, test="Chisq")

ctable = coef(summary(log_Reg_Model))
ctable
t1$Pred <- predict(log_Reg_Model, newdata = t1, type = "response")

train_tab <- table(t1$DRV_CLAIM_STATUS, t1$Pred>0.9)
train_tab
sum(diag(train_tab))/sum(train_tab)
t1_sensitivity=train_tab[1,1]/sum(train_tab[1,])
t1_sensitivity
t1_specificity=train_tab[2,2]/sum(train_tab[2,])
t1_specificity

t2$Pred <- predict(log_Reg_Model, newdata = t2, type = "response")
test_tab <- table(t2$DRV_CLAIM_STATUS, t2$Pred>0.9)
test_tab
sum(diag(test_tab))/sum(test_tab)
t2_sensitivity=test_tab[1,1]/sum(test_tab[1,])
t2_sensitivity
t2_specificity=test_tab[2,2]/sum(test_tab[2,])
t2_specificity

train_LR_predobj <- prediction(t1$Pred, t1$DRV_CLAIM_STATUS)
train_LR_perf <- performance(train_LR_predobj, "tpr", "fpr")
plot(train_LR_perf)
train_LR_KS <- max(attr(train_LR_perf, 'y.values')[[1]]-attr(train_LR_perf, 'x.values')[[1]])
train_LR_auc <- performance(train_LR_predobj,"auc"); 
train_LR_auc <- as.numeric(train_LR_auc@y.values)
train_LR_gini = ineq(t1$Pred, type="Gini")
train_LR_auc
train_LR_KS
train_LR_gini

test_LR_predobj <- prediction(t2$Pred, t2$DRV_CLAIM_STATUS)
test_LR_perf <- performance(test_LR_predobj, "tpr", "fpr")
plot(test_LR_perf)
test_LR_KS <- max(attr(test_LR_perf, 'y.values')[[1]]-attr(test_LR_perf, 'x.values')[[1]])
test_LR_auc <- performance(test_LR_predobj,"auc"); 
test_LR_auc <- as.numeric(test_LR_auc@y.values)
test_LR_gini = ineq(t2$Pred, type="Gini")
test_LR_auc
test_LR_KS
test_LR_gini

#Naive Bayes
t5=train_smote1
t6=test
NB = naivebayes::naive_bayes(t5$DRV_CLAIM_STATUS ~., data = t5, laplace=1)
NB
t5$pred = predict(NB, t5, type = "class")
train_tab.NB = table(t5[,1], t5$pred)
train_tab.NB
sum(diag(train_tab.NB))/sum(train_tab.NB)
t5_sensitivity=train_tab.NB[1,1]/sum(train_tab.NB[1,])
t5_sensitivity
t5_specificity=train_tab.NB[2,2]/sum(train_tab.NB[2,])
t5_specificity

t6$pred = predict(NB, t6, type = "class")
test_tab.NB = table(t6$DRV_CLAIM_STATUS, t6$pred)
test_tab.NB
sum(diag(test_tab.NB))/sum(test_tab.NB)
t6_sensitivity=test_tab.NB[1,1]/sum(test_tab.NB[1,])
t6_sensitivity
t6_specificity=test_tab.NB[2,2]/sum(test_tab.NB[2,])
t6_specificity

train_NB_predobj <- prediction(as.numeric(t5$pred), as.numeric(t5$DRV_CLAIM_STATUS))
train_NB_perf <- performance(train_NB_predobj, "tpr", "fpr")
plot(train_NB_perf)
train_NB_KS <- max(attr(train_NB_perf, 'y.values')[[1]]-attr(train_NB_perf, 'x.values')[[1]])
train_NB_auc <- performance(train_NB_predobj,"auc"); 
train_NB_auc <- as.numeric(train_NB_auc@y.values)
train_NB_gini = ineq(t5$pred, type="Gini")
train_NB_auc
train_NB_KS
train_NB_gini

test_NB_predobj <- prediction(as.numeric(t6$pred), as.numeric(t6$DRV_CLAIM_STATUS))
test_NB_perf <- performance(test_NB_predobj, "tpr", "fpr")
plot(test_NB_perf)
test_NB_KS <- max(attr(test_NB_perf, 'y.values')[[1]]-attr(test_NB_perf, 'x.values')[[1]])
test_NB_auc <- performance(test_NB_predobj,"auc"); 
test_NB_auc <- as.numeric(test_NB_auc@y.values)
test_NB_gini = ineq(t6$pred, type="Gini")
test_NB_auc
test_NB_KS
test_NB_gini

#Random Forest
train2=train_smote1
test2=test
rndForest1=randomForest(train2$DRV_CLAIM_STATUS~.,data=train2,nTree=51,mtry=6,nodesize=5,importance=TRUE)
print(rndForest1)
print(rndForest1$err.rate)
plot(rndForest1)
importance(rndForest1)
rndForest2=tuneRF(x=train2[,-1],y=train2$DRV_CLAIM_STATUS,mtryStart=6,step=1.5,ntreeTry=25,
                  improve = 0.001,nodesize=5,trace=TRUE,plot=TRUE,dobest=TRUE,importance=TRUE)

TrndForest=randomForest(train2$DRV_CLAIM_STATUS~.,data=train2,nTree=25,mtry=2,nodesize=5,importance=TRUE)
print(TrndForest)
# measuring model performance of random forest model 
train2$prediction=predict(TrndForest,train2,type="class")
train2$prob=predict(TrndForest,train2,type="prob")
tbl_train2=table(train2$DRV_CLAIM_STATUS,train2$prediction)
print(tbl_train2)
print((tbl_train2[1,1]+tbl_train2[2,2])/sum(tbl_train2))

decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}      #creating decile function

train2$deciles <- decile(train2$prob[,2])
train2_DT = data.table(train2)
rank2 <- train2_DT[, list(cnt=length(train2$DRV_CLAIM_STATUS),
                          cnt_resp=sum(DRV_CLAIM_STATUS == "CLOSED"),
                          cnt_non_resp = sum(DRV_CLAIM_STATUS == "REJECTED")),
                   by=deciles][order(-deciles)]
rank2
rank2$rrate <- round(rank2$cnt_resp / rank2$cnt,4)*100 #in %
rank2$cum_resp <- cumsum(rank2$cnt_resp)
rank2$cum_non_resp <- cumsum(rank2$cnt_non_resp)
rank2$cum_rel_resp <- round(rank2$cum_resp / sum(rank2$cnt_resp),4)*100 #in %
rank2$cum_rel_non_resp <- round(rank2$cum_non_resp / sum(rank2$cnt_non_resp),4)*100 #in %
rank2$ks <- abs(rank2$cum_rel_resp - rank2$cum_rel_non_resp)
predobj1 <- prediction(train2$prob[,2], train2$DRV_CLAIM_STATUS)
perf1 <- performance(predobj1, "tpr", "fpr")
plot(perf1)
KS1 <- max(attr(perf1, 'y.values')[[1]]-attr(perf1, 'x.values')[[1]])
auc1 <- performance(predobj1,"auc"); 
auc1 <- as.numeric(auc1@y.values)
gini1 = ineq(train2$prob[,2], type="Gini")
auc1
KS1
gini1
#test the model on Test data
test2$prediction=predict(TrndForest,test2,type="class")
test2$prob=predict(TrndForest,test2,type="prob")
tbl_test2=table(test2$DRV_CLAIM_STATUS,test2$prediction) #confusion matrix
print(tbl_test2)
print((tbl_test2[1,1]+tbl_test2[2,2])/sum(tbl_test2))
test2$deciles <- decile(test2$prob[,2])
test2_DT = data.table(test2)
trank2 <- test2_DT[, list(cnt=length(test2$DRV_CLAIM_STATUS),
                           cnt_resp=sum(test2$DRV_CLAIM_STATUS == "CLOSED"),
                           cnt_non_resp = sum(test2$DRV_CLAIM_STATUS == "REJECTED")),
                    by=deciles][order(-deciles)]
trank2
trank2$rrate <- round(trank2$cnt_resp / trank2$cnt,4)*100 #in %
trank2$cum_resp <- cumsum(trank2$cnt_resp)
trank2$cum_non_resp <- cumsum(trank2$cnt_non_resp)
trank2$cum_rel_resp <- round(trank2$cum_resp / sum(trank2$cnt_resp),4)*100 #in %
trank2$cum_rel_non_resp <- round(trank2$cum_non_resp / sum(trank2$cnt_non_resp),4)*100 #in %
trank2$ks <- abs(trank2$cum_rel_resp - trank2$cum_rel_non_resp)
tpredobj1 <- prediction(test2$prob[,2], test2$DRV_CLAIM_STATUS)
tperf1 <- performance(tpredobj1, "tpr", "fpr")
plot(tperf1)
tKS1 <- max(attr(tperf1, 'y.values')[[1]]-attr(tperf1, 'x.values')[[1]])
tauc1 <- performance(tpredobj1,"auc"); 
tauc1 <- as.numeric(tauc1@y.values)
tgini1 = ineq(test2$prob[,2], type="Gini")
tauc1
tKS1
tgini1

#Bagging
library(ipred)
t11=train_smote1
t12=test
BAGModel <- bagging(t11$DRV_CLAIM_STATUS~., data = t11,
                    control = rpart.control(maxdepth = 5, minsplit = 500))
BAGModel
summary(BAGModel)
t11$pred <- predict(BAGModel, t11)
trainBAG <- table(t11$DRV_CLAIM_STATUS, t11$pred)
trainBAG
sum(diag(trainBAG))/sum(trainBAG)
t11_sensitivity=trainBAG[1,1]/sum(trainBAG[1,])
t11_sensitivity
t11_specificity=trainBAG[2,2]/sum(trainBAG[2,])
t11_specificity

t12$pred <- predict(BAGModel, t12)
testBAG <- table(t12$DRV_CLAIM_STATUS, t12$pred)
testBAG
sum(diag(testBAG))/sum(testBAG)
t12_sensitivity=testBAG[1,1]/sum(testBAG[1,])
t12_sensitivity
t12_specificity=testBAG[2,2]/sum(testBAG[2,])
t12_specificity

train_bag_predobj <- prediction(as.numeric(t11$pred), as.numeric(t11$DRV_CLAIM_STATUS))
train_bag_perf <- performance(train_bag_predobj, "tpr", "fpr")
plot(train_bag_perf)
train_bag_KS <- max(attr(train_bag_perf, 'y.values')[[1]]-attr(train_bag_perf, 'x.values')[[1]])
train_bag_auc <- performance(train_bag_predobj,"auc"); 
train_bag_auc <- as.numeric(train_bag_auc@y.values)
train_bag_gini = ineq(t11$pred, type="Gini")
train_bag_auc
train_bag_KS
train_bag_gini

test_bag_predobj <- prediction(as.numeric(t12$pred),as.numeric(t12$DRV_CLAIM_STATUS))
test_bag_perf <- performance(train_bag_predobj, "tpr", "fpr")
plot(test_bag_perf)
test_bag_KS <- max(attr(test_bag_perf, 'y.values')[[1]]-attr(test_bag_perf, 'x.values')[[1]])
test_bag_auc <- performance(test_bag_predobj,"auc"); 
test_bag_auc <- as.numeric(test_bag_auc@y.values)
test_bag_gini = ineq(t12$pred, type="Gini")
test_bag_auc
test_bag_KS
test_bag_gini

