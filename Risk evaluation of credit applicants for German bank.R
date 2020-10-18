
library("dplyr")
library("psych")
library("rpart")
library("ROCR")
library("dplyr")
library("rpart.plot")
library('lift')
library("xlsx")
library('randomForest')
library('C50')


#Deleting the columns of NEW_CAR , USED_CAR , FURNITURE , EDUCATION , RETRAINING from the German Credit dataset
GermanData <- within(GermanCredit_assgt1_S19, rm(NEW_CAR, USED_CAR,FURNITURE,EDUCATION,RETRAINING))
GermanData <- GermanData[, -c(5)]
GermanData <- within(GermanData, rm('OBS#'))
GermanData <- GermanData[, -c(24)]
GermanData$PERSONAL_STATUS[is.na(GermanData$PERSONAL_STATUS)==TRUE] <- 1

#Mean obtained for Age column
mean(na.omit(GermanData$AGE))
#Replacing of NA's in Age column with the value 35 as obtained from the previous step
GermanData$AGE[is.na(GermanData$AGE)] <- 35

#Grouping of NUM_CREDITS = 2 , 3 and 4 into 2 for having enough samples to build model
GermanData$NUM_CREDITS[GermanData$NUM_CREDITS ==2 | GermanData$NUM_CREDITS ==3 | GermanData$NUM_CREDITS ==4] <- "2"


#Conversion of different variables to factors
GermanData$CHK_ACCT<-as.factor(GermanData$CHK_ACCT)
GermanData$HISTORY<-as.factor(GermanData$HISTORY)
GermanData$SAV_ACCT<-as.factor(GermanData$SAV_ACCT)
GermanData$EMPLOYMENT<-as.factor(GermanData$EMPLOYMENT)
GermanData$PERSONAL_STATUS<-as.factor(GermanData$PERSONAL_STATUS)
GermanData$GUARANTOR <- as.factor(GermanData$GUARANTOR)
GermanData$PRESENT_RESIDENT <- as.factor(GermanData$PRESENT_RESIDENT)
GermanData$REAL_ESTATE <- as.factor(GermanData$REAL_ESTATE)
GermanData$PROP_UNKN_NONE <- as.factor(GermanData$PROP_UNKN_NONE)
GermanData$OTHER_INSTALL <- as.factor(GermanData$OTHER_INSTALL)
GermanData$RENT <- as.factor(GermanData$RENT)
GermanData$OWN_RES <- as.factor(GermanData$OWN_RES)
GermanData$NUM_CREDITS <- as.factor(GermanData$NUM_CREDITS)
GermanData$JOB <- as.factor(GermanData$JOB)
GermanData$TELEPHONE <- as.factor(GermanData$TELEPHONE)
GermanData$FOREIGN <- as.factor(GermanData$FOREIGN)
GermanData$`CO-APPLICANT` <- as.factor(GermanData$`CO-APPLICANT`)
GermanData$NUM_DEPENDENTS<-as.factor(GermanData$NUM_DEPENDENTS)

#Conversion of Response variable to Good/Bad
GermanData$RESPONSE[GermanData$RESPONSE==1] <- "Good"
GermanData$RESPONSE[GermanData$RESPONSE==0] <- "Bad"
GermanData$RESPONSE<-(as.factor(GermanData$RESPONSE))

#Q2.b)


#Models
#Model 1 : Gini Index
rpModel1=rpart(RESPONSE ~ ., data=GermanData, method="class",parms = list(split ='gini'))
rpart.plot::prp(rpModel1, type=2, extra=1)
summary(rpModel1)
pred1=predict(rpModel1, GermanData, type='class')
table(pred = pred1, true=GermanData$RESPONSE)
mean(pred1==GermanData$RESPONSE)

#Model 2 : Information Gain
rpModel2=rpart(RESPONSE ~ ., data=GermanData, method="class",parms = list(split ='information'))
rpart.plot::prp(rpModel2, type=2, extra=1)
summary(rpModel2)
pred2=predict(rpModel2, GermanData, type='class')
table(pred = pred2, true=GermanData$RESPONSE)
mean(pred2==GermanData$RESPONSE)


#Model 3 : Gini Index with Minsplit = 18
rpModel3=rpart(RESPONSE ~ ., data=GermanData, method="class",parms = list(split ='gini'), control= rpart.control(minsplit=18))
rpart.plot::prp(rpModel3, type=2, extra=1)
summary(rpModel3)
pred3=predict(rpModel3, GermanData, type='class')
table(pred = pred3, true=GermanData$RESPONSE)
mean(pred3==GermanData$RESPONSE)


#Model 4 : Gini Index with Minsplit = 18 and MaxDepth = 8
rpmodel4 = rpart(RESPONSE ~ ., data=GermanData, method="class",parms = list(split ='gini'), control= rpart.control(minsplit=18,maxdepth = 8 ))
rpart.plot::prp(rpmodel4, type=2, extra=1)
summary(rpmodel4)
plotcp(rpmodel4)
pred4=predict(rpmodel4, GermanData, type='class')
table(pred = pred4, true = GermanData$RESPONSE)
mean(pred4==GermanData$RESPONSE)


#Model 5 : Gini Index with CP = 0.007 and MaxDepth = 8
rpmodel5 = rpart(RESPONSE ~ ., data=GermanData, method="class",parms = list(split ='gini'),control = rpart.control(cp=.007,maxdepth = 8))
rpart.plot::prp(rpmodel5, type=2, extra=1)
summary(rpmodel5)
plotcp(rpmodel5)
pred5=predict(rpmodel5, GermanData, type='class')
table(pred = pred5, true = GermanData$RESPONSE)
mean(pred5==GermanData$RESPONSE)

#Model 6 : Gini Index with CP = 0.0085 , MinSplit = 8 and MaxDepth = 12
rpmodel6 = rpart(RESPONSE ~ ., data=GermanData, method="class",parms = list(split ='gini'),control = rpart.control(cp=0.0085,minsplit = 8,maxdepth = 12))
rpart.plot::prp(rpmodel6, type=2, extra=1)
summary(rpmodel6)
plotcp(rpmodel6)
pred6=predict(rpmodel6, GermanData, type='class')
table(pred = pred6, true = GermanData$RESPONSE)
#confusionMatrix(pred6,GermanData$RESPONSE)
mean(pred6==GermanData$RESPONSE)

#Q2 c)
#Lift chart
predGermanDataProb=predict(rpmodel6, GermanData, type='prob')
head(predGermanDataProb)
GermanDataSc <- subset(GermanData, select=c("RESPONSE"))
GermanDataSc$score<-predGermanDataProb[, 2]
GermanDataSc<-GermanDataSc[order(GermanDataSc$score, decreasing=TRUE),]
levels(GermanDataSc$RESPONSE)[1]<-0
levels(GermanDataSc$RESPONSE)[2]<-1
GermanDataSc$RESPONSE<-as.numeric(as.character(GermanDataSc$RESPONSE))
GermanDataSc$cumDefault<-cumsum(GermanDataSc$RESPONSE)
plot(seq(nrow(GermanDataSc)), GermanDataSc$cumDefault,type = "l", xlab='#cases', ylab='#Good credit')

#Identify the count of Good and Bad credit cases
table(GermanData$RESPONSE)

GermanDataSc["bucket"]<-ntile(-GermanDataSc[,"score"], 10)  
decGroups<-group_by(GermanDataSc, bucket)
decLifts<-summarise(decGroups, count=n(), numDefaults=sum(RESPONSE))
decLifts<-decLifts %>% mutate(defRate=numDefaults/count, cumDefRate=cumsum(numDefaults)/cumsum(count),       lift=cumDefRate/(sum(numDefaults)/sum(count)) )
plot(decLifts$bucket, decLifts$lift, xlab="deciles", ylab="Cumulative Decile Lift", type="l")
barplot(decLifts$numDefaults, main="numDefaults by decile", xlab="deciles")

plotLift(GermanDataSc$score, GermanDataSc$RESPONSE)
TopDecileLift(GermanDataSc$score, GermanDataSc$RESPONSE)
GermanDataPred<- prediction(GermanDataSc$score, GermanDataSc$RESPONSE)
liftPerf<-performance(GermanDataPred, "lift", "rpp")
plot(liftPerf, main="Lift chart")
gainPerf<-performance(GermanDataPred, "tpr", "rpp")
plot(gainPerf, main="Gain chart")


#Q3 a)

#Splitting the GermanData in 50-50 training and testing data
nr=nrow(GermanData)
trnIndex = sample(1:nr, size = round(0.5*nr), replace=FALSE)
GermanDataTrn=GermanData[trnIndex,]  
GermanDataTst = GermanData[-trnIndex,]  

#Training Model 1 : Gini Index 
trnmodel1 <- rpart(RESPONSE ~ ., data=GermanDataTrn, method="class")
rpart.plot::prp(trnmodel1, type=2, extra=1)
summary(trnmodel1)
plotcp(trnmodel1)
trnpred1=predict(trnmodel1, GermanDataTrn, type='class')
table(pred = trnpred1, true = GermanDataTrn$RESPONSE)
mean(trnpred1==GermanDataTrn$RESPONSE)

#Training Model 2 : Information Gain
trnmodel2 <- rpart(RESPONSE ~ ., data=GermanDataTrn, method="class",parms = list(split ='information'))
rpart.plot::prp(trnmodel2, type=2, extra=1)
summary(trnmodel2)
plotcp(trnmodel2)
trnpred2=predict(trnmodel2, GermanDataTrn, type='class')
table(pred = trnpred2, true = GermanDataTrn$RESPONSE)
mean(trnpred2==GermanDataTrn$RESPONSE)

#Training Model 3 : Gini Index with Minsplit = 18
trnmodel3 <- rpart(RESPONSE ~ ., data=GermanDataTrn, method="class",parms = list(split ='gini'), control= rpart.control(minsplit=18))
rpart.plot::prp(trnmodel3, type=2, extra=1)
summary(trnmodel3)
trnpred3=predict(trnmodel3, GermanDataTrn, type='class')
table(pred = trnpred3, true=GermanDataTrn$RESPONSE)
mean(trnpred3==GermanDataTrn$RESPONSE)

#Training Model 4 : Gini Index with Minsplit = 18 and MaxDepth = 8
trnmodel4 <- rpart(RESPONSE ~ ., data=GermanDataTrn, method="class",parms = list(split ='gini'), control= rpart.control(minsplit=18,maxdepth = 8 ))
rpart.plot::prp(trnmodel4, type=2, extra=1)
summary(trnmodel4)
plotcp(trnmodel4)
trnpred4=predict(trnmodel4, GermanDataTrn, type='class')
table(pred = trnpred4, true = GermanDataTrn$RESPONSE)
mean(trnpred4==GermanDataTrn$RESPONSE)

#Training Model 5 : Gini Index with CP = 0.007 and MaxDepth = 8
trnmodel5 <- rpart(RESPONSE ~ ., data=GermanDataTrn, method="class",parms = list(split ='gini'),control = rpart.control(cp=.007,maxdepth = 8))
rpart.plot::prp(trnmodel5, type=2, extra=1)
summary(trnmodel5)
plotcp(trnmodel5)
trnpred5=predict(trnmodel5, GermanDataTrn, type='class')
table(pred = trnpred5, true = GermanDataTrn$RESPONSE)
mean(trnpred5==GermanDataTrn$RESPONSE)

#Training Model 6 : Gini Index with CP = 0.0085 , MinSplit = 8 and MaxDepth = 12
trnmodel6 <- rpart(RESPONSE ~ ., data=GermanDataTrn, method="class",parms = list(split ='gini'),control = rpart.control(cp=0.0085,minsplit = 8,maxdepth = 12))
rpart.plot::prp(trnmodel6, type=2, extra=1)
summary(trnmodel6)
plotcp(trnmodel6)
trnpred6=predict(trnmodel6, GermanDataTrn, type='class')
table(pred = trnpred6, true = GermanDataTrn$RESPONSE)
mean(trnpred6==GermanDataTrn$RESPONSE)


#Lift Chart
predGermanDataProb=predict(trnmodel6, GermanData, type='prob')
head(predGermanDataProb)
GermanDataSc <- subset(GermanData, select=c("RESPONSE"))
GermanDataSc$score<-predGermanDataProb[, 2]
GermanDataSc<-GermanDataSc[order(GermanDataSc$score, decreasing=TRUE),]
levels(GermanDataSc$RESPONSE)[1]<-0
levels(GermanDataSc$RESPONSE)[2]<-1
GermanDataSc$RESPONSE<-as.numeric(as.character(GermanDataSc$RESPONSE))
GermanDataSc$cumDefault<-cumsum(GermanDataSc$RESPONSE)
plot(seq(nrow(GermanDataSc)), GermanDataSc$cumDefault,type = "l", xlab='#cases', ylab='#Good credit')

#Identify the count of Good and Bad credit cases
table(GermanData$RESPONSE)

GermanDataSc["bucket"]<-ntile(-GermanDataSc[,"score"], 10)  
decGroups<-group_by(GermanDataSc, bucket)
decLifts<-summarise(decGroups, count=n(), numDefaults=sum(RESPONSE))
decLifts<-decLifts %>% mutate(defRate=numDefaults/count, cumDefRate=cumsum(numDefaults)/cumsum(count),       lift=cumDefRate/(sum(numDefaults)/sum(count)) )
plot(decLifts$bucket, decLifts$lift, xlab="deciles", ylab="Cumulative Decile Lift", type="l")
barplot(decLifts$numDefaults, main="numDefaults by decile", xlab="deciles")

plotLift(GermanDataSc$score, GermanDataSc$RESPONSE)
TopDecileLift(GermanDataSc$score, GermanDataSc$RESPONSE)
GermanDataPred<- prediction(GermanDataSc$score, GermanDataSc$RESPONSE)
liftPerf<-performance(GermanDataPred, "lift", "rpp")
plot(liftPerf, main="Lift chart")
gainPerf<-performance(GermanDataPred, "tpr", "rpp")
plot(gainPerf, main="Gain chart")

#ROC curve
scoreTst=predict(trnmodel6,GermanDataTst, type="prob")[,'Good']  
rocPredTst = prediction(scoreTst, GermanDataTst$RESPONSE, label.ordering = c('Bad', 'Good'))  
perfROCTst=performance(rocPredTst, "tpr", "fpr")
plot(perfROCTst)

#AUC Curve
accPerf = performance(rocPredTst, measure = "acc")
plot(accPerf)

#AUC value
aucPerf = performance(rocPredTst, measure = "auc")
aucPerf@y.values

#Q3 b)
#C5.0 Model 1: Default model
cModel1<- C5.0(RESPONSE ~ ., data=GermanDataTrn)
summary(cModel1)
plot(cModel1)
C50_pred1=predict(cModel1,GermanDataTrn, type='class')
table(pred = C50_pred1, true=GermanDataTrn$RESPONSE)
mean(C50_pred1==GermanDataTrn$RESPONSE)

#C5.0 Model 2: Model with mincases = 5
cModel2<- C5.0(RESPONSE ~ ., data=GermanDataTrn, method="class",control = C5.0Control( minCases = 5))
summary(cModel2)
plot(cModel2)
C50_pred2=predict(cModel2,GermanDataTrn, type='class')
table(pred = C50_pred2, true=GermanDataTrn$RESPONSE)
mean(C50_pred2==GermanDataTrn$RESPONSE)

#C5.0 Model 2: Model with mincases = 25
cModel3<- C5.0(RESPONSE ~ ., data=GermanDataTrn, method="class",control = C5.0Control( minCases = 25))
plot(cModel3)
summary(cModel3)
C50_pred3=predict(cModel3,GermanDataTrn, type='class')
table(pred = C50_pred3, true=GermanDataTrn$RESPONSE)
mean(C50_pred3==GermanDataTrn$RESPONSE)

#Lift curve based on Default model
predGermanDataProb1=predict(cModel1, GermanDataTrn, type='prob')
head(predGermanDataProb1)
GermanDataSc1 <- subset(GermanDataTrn, select=c("RESPONSE"))
GermanDataSc1$score<-predGermanDataProb1[, 2]
View(GermanDataSc1)
GermanDataSc1<-GermanDataSc1[order(GermanDataSc1$score, decreasing=TRUE),]
levels(GermanDataSc1$RESPONSE)[1]<-0
levels(GermanDataSc1$RESPONSE)[2]<-1
GermanDataSc1$RESPONSE<-as.numeric(as.character(GermanDataSc1$RESPONSE))
GermanDataSc1$cumDefault<-cumsum(GermanDataSc1$RESPONSE)
plot(seq(nrow(GermanDataSc1)), GermanDataSc1$cumDefault,type = "l", xlab='#cases', ylab='#Good Cases')

#ROC curve
scoreTst1=predict(cModel1,GermanDataTst, type="prob")[,'Good']  
rocPredTst1 = prediction(scoreTst1, GermanDataTst$RESPONSE, label.ordering = c('Bad', 'Good'))  
perfROCTst1=performance(rocPredTst1, "tpr", "fpr")
plot(perfROCTst1)

#AUC curve
accPerf = performance(rocPredTst1, measure = "acc")
plot(accPerf)

#AUC value
aucPerf = performance(rocPredTst1, measure = "auc")
aucPerf@y.values


#Q3 c)

#Setting seed to 123      
set.seed(123)
nr=nrow(GermanData)
trnIndex1 = sample(1:nr, size = round(0.5*nr), replace=FALSE)
GermanDataTrn1=GermanData[trnIndex1,]  
GermanDataTst1 = GermanData[-trnIndex1,] 

#Model with seed set to 123 : Gini Index with CP = 0.0085 , MinSplit = 8 and MaxDepth = 12
rpartModel123<- rpart(RESPONSE ~ ., data=GermanDataTrn1, method="class",parms = list(split ='gini'),control = rpart.control(cp=0.0085,minsplit = 8,maxdepth = 12))
summary(rpartModel123)
plot(rpartModel123)
rpart_pred123=predict(rpartModel123,GermanDataTrn1, type='class')
table(pred = rpart_pred123, true=GermanDataTrn1$RESPONSE)
mean(rpart_pred123==GermanDataTrn1$RESPONSE)


#Setting seed to 555 
set.seed(555)
nr=nrow(GermanData)
trnIndex2 = sample(1:nr, size = round(0.5*nr), replace=FALSE)
GermanDataTrn2=GermanData[trnIndex2,]  
GermanDataTst2 = GermanData[-trnIndex2,] 

#Model with seed set to 555 : Gini Index with CP = 0.0085 , MinSplit = 8 and MaxDepth = 12
rpartModel555<- rpart(RESPONSE ~ ., data=GermanDataTrn1, method="class",parms = list(split ='gini'),control = rpart.control(cp=0.0085,minsplit = 8,maxdepth = 12))
summary(rpartModel555)
plot(rpartModel555)
rpart_pred555=predict(rpartModel555,GermanDataTrn2, type='class')
table(pred = rpart_pred555, true=GermanDataTrn2$RESPONSE)
mean(rpart_pred555==GermanDataTrn2$RESPONSE)


#Setting seed to 222 
set.seed(222)
nr=nrow(GermanData)
trnIndex3 = sample(1:nr, size = round(0.5*nr), replace=FALSE)
GermanDataTrn3=GermanData[trnIndex3,]  
GermanDataTst3 = GermanData[-trnIndex3,] 

#Model with seed set to 222 : Gini Index with CP = 0.0085 , MinSplit = 8 and MaxDepth = 12
rpartModel222<- rpart(RESPONSE ~ ., data=GermanDataTrn3, method="class",parms = list(split ='gini'),control = rpart.control(cp=0.0085,minsplit = 8,maxdepth = 12))
summary(rpartModel222)
plot(rpartModel222)
rpart_pred222=predict(rpartModel222,GermanDataTrn3, type='class')
table(pred = rpart_pred222, true=GermanDataTrn3$RESPONSE)
mean(rpart_pred222==GermanDataTrn3$RESPONSE)

#Setting seed to 2000 
set.seed(2000)
nr=nrow(GermanData)
trnIndex4 = sample(1:nr, size = round(0.5*nr), replace=FALSE)
GermanDataTrn4=GermanData[trnIndex4,]  
GermanDataTst4 = GermanData[-trnIndex4,] 

#Model with seed set to 2000 : Gini Index with CP = 0.0085 , MinSplit = 8 and MaxDepth = 12
rpartModel2000<- rpart(RESPONSE ~ ., data=GermanDataTrn4, method="class",parms = list(split ='gini'),control = rpart.control(cp=0.0085,minsplit = 8,maxdepth = 12))
summary(rpartModel2000)
plot(rpartModel2000)
rpart_pred2000=predict(rpartModel2000,GermanDataTrn4, type='class')
table(pred = rpart_pred2000, true=GermanDataTrn4$RESPONSE)
mean(rpart_pred2000==GermanDataTrn4$RESPONSE)


#Q3 d)
#Obtain variable importance from rpart tree and C5.0 tree
summary(trnmodel6)
summary(cModel1)

#Q3 e)

#Splitting the GermanData in 50-50 training and testing data
nr=nrow(GermanData)
trnIndex = sample(1:nr, size = round(0.5*nr), replace=FALSE)
GermanDataTrn=GermanData[trnIndex,]  
GermanDataTst = GermanData[-trnIndex,] 

#Model 1 : Information gain
rpmodelTrn1 <- rpart(RESPONSE ~ ., data=GermanDataTrn, method="class",parms = list(split ='information'))
rpart.plot::prp(rpmodelTrn1, type=2, extra=1)
summary(rpmodelTrn1)
plotcp(rpmodelTrn1)
predTrn1=predict(rpmodelTrn1, GermanDataTrn, type='class')
table(pred = predTrn1, true = GermanDataTrn$RESPONSE)
mean(predTrn1==GermanDataTrn$RESPONSE)

predTst1=predict(rpmodelTrn1, GermanDataTst, type='class')
table(pred = predTst1, true=GermanDataTst$RESPONSE)
mean(predTst1==GermanDataTst$RESPONSE)


#Model 2 : Gini Index with CP = 0.0085 , MinSplit = 8 and MaxDepth = 12
rpmodelTrn2 <- rpart(RESPONSE ~ ., data=GermanDataTrn, method="class",parms = list(split ='gini'),control = rpart.control(cp=0.0085,minsplit = 8,maxdepth = 12))
rpart.plot::prp(rpmodelTrn2, type=2, extra=1)
summary(rpmodelTrn2)
plotcp(rpmodelTrn2)
predTrn2=predict(rpmodelTrn2, GermanDataTrn, type='class')
table(pred = predTrn2, true = GermanDataTrn$RESPONSE)
mean(predTrn2==GermanDataTrn$RESPONSE)

predTst2=predict(rpmodelTrn2, GermanDataTst, type='class')
table(pred = predTst2, true=GermanDataTst$RESPONSE)
mean(predTst2==GermanDataTst$RESPONSE)


#Model 3 : Default model
cModelTrn1<- C5.0(RESPONSE ~ ., data=GermanDataTrn)
summary(cModelTrn1)
plot(cModelTrn1)
C50_predTrn1=predict(cModelTrn1,GermanDataTrn, type='class')
table(pred = C50_predTrn1, true=GermanDataTrn$RESPONSE)
mean(C50_predTrn1==GermanDataTrn$RESPONSE)

C50_predTst1=predict(cModelTrn1,GermanDataTst, type='class')
table(pred = C50_predTst1, true=GermanDataTst$RESPONSE)
mean(C50_predTst1==GermanDataTst$RESPONSE)

#Model 4 : Model with mincases = 25

cModelTrn2<- C5.0(RESPONSE ~ ., data=GermanDataTrn, method="class",control = C5.0Control( minCases = 25))
summary(cModelTrn2)
plot(cModelTrn2)
C50_predTrn2=predict(cModelTrn2,GermanDataTrn, type='class')
table(pred = C50_predTrn2, true=GermanDataTrn$RESPONSE)
mean(C50_predTrn2==GermanDataTrn$RESPONSE)

C50_predTst2=predict(cModelTrn2,GermanDataTst, type='class')
table(pred = C50_predTst2, true=GermanDataTst$RESPONSE)
mean(C50_predTst2==GermanDataTst$RESPONSE)


#Splitting the GermanData in 70-30 training and testing data
nr=nrow(GermanData)
trnIndex1 = sample(1:nr, size = round(0.7*nr), replace=FALSE)
GermanDataTrn_70=GermanData[trnIndex1,]  
GermanDataTst_30 = GermanData[-trnIndex1,] 

#Model 1 : Information gain
rpmodelTrn1 <- rpart(RESPONSE ~ ., data=GermanDataTrn_70, method="class",parms = list(split ='information'))
rpart.plot::prp(rpmodelTrn1, type=2, extra=1)
summary(rpmodelTrn1)
plotcp(rpmodelTrn1)
predTrn1=predict(rpmodelTrn1, GermanDataTrn_70, type='class')
table(pred = predTrn1, true = GermanDataTrn_70$RESPONSE)
mean(predTrn1==GermanDataTrn_70$RESPONSE)

predTst1=predict(rpmodelTrn1, GermanDataTst_30, type='class')
table(pred = predTst1, true=GermanDataTst_30$RESPONSE)
mean(predTst1==GermanDataTst_30$RESPONSE)


#Model 2 : Gini Index with CP = 0.0085 , MinSplit = 8 and MaxDepth = 12
rpmodelTrn2 <- rpart(RESPONSE ~ ., data=GermanDataTrn_70, method="class",parms = list(split ='gini'),control = rpart.control(cp=0.0085,minsplit = 8,maxdepth = 12))
rpart.plot::prp(rpmodelTrn2, type=2, extra=1)
summary(rpmodelTrn2)
plotcp(rpmodelTrn2)
predTrn2=predict(rpmodelTrn2, GermanDataTrn_70, type='class')
table(pred = predTrn2, true = GermanDataTrn_70$RESPONSE)
mean(predTrn2==GermanDataTrn_70$RESPONSE)

predTst2=predict(rpmodelTrn2, GermanDataTst_30, type='class')
table(pred = predTst2, true=GermanDataTst_30$RESPONSE)
mean(predTst2==GermanDataTst_30$RESPONSE)


#Model 3 : Default model
cModelTrn1<- C5.0(RESPONSE ~ ., data=GermanDataTrn_70)
summary(cModelTrn1)
plot(cModelTrn1)
C50_predTrn1=predict(cModelTrn1,GermanDataTrn_70, type='class')
table(pred = C50_predTrn1, true=GermanDataTrn_70$RESPONSE)
mean(C50_predTrn1==GermanDataTrn_70$RESPONSE)

C50_predTst1=predict(cModelTrn1,GermanDataTst_30, type='class')
table(pred = C50_predTst1, true=GermanDataTst_30$RESPONSE)
mean(C50_predTst1==GermanDataTst_30$RESPONSE)

#Model 4 : Model with mincases = 25

cModelTrn2<- C5.0(RESPONSE ~ ., data=GermanDataTrn_70, method="class",control = C5.0Control( minCases = 25))
summary(cModelTrn2)
plot(cModelTrn2)
C50_predTrn2=predict(cModelTrn2,GermanDataTrn_70, type='class')
table(pred = C50_predTrn2, true=GermanDataTrn_70$RESPONSE)
mean(C50_predTrn2==GermanDataTrn_70$RESPONSE)

C50_predTst2=predict(cModelTrn2,GermanDataTst_30, type='class')
table(pred = C50_predTst2, true=GermanDataTst_30$RESPONSE)
mean(C50_predTst2==GermanDataTst_30$RESPONSE)

#Splitting the GermanData in 80-20 training and testing data
nr=nrow(GermanData)
trnIndex2 = sample(1:nr, size = round(0.8*nr), replace=FALSE)
GermanDataTrn_80=GermanData[trnIndex2,]  
GermanDataTst_20 = GermanData[-trnIndex2,] 

#Model 1 : Information gain
rpmodelTrn1 <- rpart(RESPONSE ~ ., data=GermanDataTrn_80, method="class",parms = list(split ='information'))
rpart.plot::prp(rpmodelTrn1, type=2, extra=1)
summary(rpmodelTrn1)
plotcp(rpmodelTrn1)
predTrn1=predict(rpmodelTrn1, GermanDataTrn_80, type='class')
table(pred = predTrn1, true = GermanDataTrn_80$RESPONSE)
mean(predTrn1==GermanDataTrn_80$RESPONSE)

predTst1=predict(rpmodelTrn1, GermanDataTst_20, type='class')
table(pred = predTst1, true=GermanDataTst_20$RESPONSE)
mean(predTst1==GermanDataTst_20$RESPONSE)


#Model 2 : Gini Index with CP = 0.0085 , MinSplit = 8 and MaxDepth = 12
rpmodelTrn2 <- rpart(RESPONSE ~ ., data=GermanDataTrn_80, method="class",parms = list(split ='gini'),control = rpart.control(cp=0.0085,minsplit = 8,maxdepth = 12))
rpart.plot::prp(rpmodelTrn2, type=2, extra=1)
summary(rpmodelTrn2)
plotcp(rpmodelTrn2)
predTrn2=predict(rpmodelTrn2, GermanDataTrn_80, type='class')
table(pred = predTrn2, true = GermanDataTrn_80$RESPONSE)
mean(predTrn2==GermanDataTrn_80$RESPONSE)

predTst2=predict(rpmodelTrn2, GermanDataTst_20, type='class')
table(pred = predTst2, true=GermanDataTst_20$RESPONSE)
mean(predTst2==GermanDataTst_20$RESPONSE)


#Model 3 : Default model
cModelTrn1<- C5.0(RESPONSE ~ ., data=GermanDataTrn_80)
summary(cModelTrn1)
plot(cModelTrn1)
C50_predTrn1=predict(cModelTrn1,GermanDataTrn_80, type='class')
table(pred = C50_predTrn1, true=GermanDataTrn_80$RESPONSE)
mean(C50_predTrn1==GermanDataTrn_80$RESPONSE)

C50_predTst1=predict(cModelTrn1,GermanDataTst_20, type='class')
table(pred = C50_predTst1, true=GermanDataTst_20$RESPONSE)
mean(C50_predTst1==GermanDataTst_20$RESPONSE)

#Model 4 : Model with mincases = 25

cModelTrn2<- C5.0(RESPONSE ~ ., data=GermanDataTrn_80, method="class",control = C5.0Control( minCases = 25))
summary(cModelTrn2)
plot(cModelTrn2)
C50_predTrn2=predict(cModelTrn2,GermanDataTrn_80, type='class')
table(pred = C50_predTrn2, true=GermanDataTrn_80$RESPONSE)
mean(C50_predTrn2==GermanDataTrn_80$RESPONSE)

C50_predTst2=predict(cModelTrn2,GermanDataTst_20, type='class')
table(pred = C50_predTst2, true=GermanDataTst_20$RESPONSE)
mean(C50_predTst2==GermanDataTst_20$RESPONSE)


#Q4 a)

#We have identified the Gini Index with CP = 0.0085 , MinSplit = 8 and MaxDepth = 12 as the best model.
#Setting the threshold value to 0.5
CTHRESH=0.5
#Training accuracy
predProbTstt=predict(rpmodelTrn2,GermanDataTrn_70, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTrn_70$RESPONSE)
mean(predTstt==GermanDataTrn_70$RESPONSE)

#Testing accuracy
predProbTstt=predict(rpmodelTrn2, GermanDataTst_30, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst_30$RESPONSE)
mean(predTstt==GermanDataTst_30$RESPONSE)


CTHRESH=0.2
#Training accuracy
predProbTstt=predict(rpmodelTrn2,GermanDataTrn_70, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTrn_70$RESPONSE)
mean(predTstt==GermanDataTrn_70$RESPONSE)

#Testing accuracy
predProbTstt=predict(rpmodelTrn2, GermanDataTst_30, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst_30$RESPONSE)
mean(predTstt==GermanDataTst_30$RESPONSE)


CTHRESH=0.7
#Training accuracy
predProbTstt=predict(rpmodelTrn2,GermanDataTrn_70, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTrn_70$RESPONSE)
mean(predTstt==GermanDataTrn_70$RESPONSE)

#Testing accuracy
predProbTstt=predict(rpmodelTrn2, GermanDataTst_30, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst_30$RESPONSE)
mean(predTstt==GermanDataTst_30$RESPONSE)

CTHRESH=0.8
#Training accuracy
predProbTstt=predict(rpmodelTrn2,GermanDataTrn_70, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTrn_70$RESPONSE)
mean(predTstt==GermanDataTrn_70$RESPONSE)

#Testing accuracy
predProbTstt=predict(rpmodelTrn2, GermanDataTst_30, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst_30$RESPONSE)
mean(predTstt==GermanDataTst_30$RESPONSE)


#Q4 b)
CTHRESH=0.83
#Training accuracy
predProbTstt=predict(rpmodelTrn2,GermanDataTrn_70, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTrn_70$RESPONSE)
mean(predTstt==GermanDataTrn_70$RESPONSE)

#Testing accuracy
predProbTstt=predict(rpmodelTrn2, GermanDataTst_30, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst_30$RESPONSE)
mean(predTstt==GermanDataTst_30$RESPONSE)



#Q4 c)

CTHRESH=0.5
#Testing accuracy for rpart
predProbTstt=predict(rpmodelTrn2, GermanDataTst, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst$RESPONSE)
mean(predTstt==GermanDataTst$RESPONSE)

#Testing accuracy for C5.0
predProbTstt=predict(cModel1, GermanDataTst, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst$RESPONSE)
mean(predTstt==GermanDataTst$RESPONSE)


CTHRESH=0.7
#Testing accuracy for rpart
predProbTstt=predict(rpmodelTrn2, GermanDataTst, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst$RESPONSE)
mean(predTstt==GermanDataTst$RESPONSE)

#Testing accuracy for C5.0
predProbTstt=predict(cModel1, GermanDataTst, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst$RESPONSE)
mean(predTstt==GermanDataTst$RESPONSE)


CTHRESH=0.8
#Testing accuracy for rpart
predProbTstt=predict(rpmodelTrn2, GermanDataTst, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst$RESPONSE)
mean(predTstt==GermanDataTst$RESPONSE)

#Testing accuracy for C5.0
predProbTstt=predict(cModel1, GermanDataTst, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst$RESPONSE)
mean(predTstt==GermanDataTst$RESPONSE)

CTHRESH=0.85
#Testing accuracy for rpart
predProbTstt=predict(rpmodelTrn2, GermanDataTst, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst$RESPONSE)
mean(predTstt==GermanDataTst$RESPONSE)

#Testing accuracy for C5.0
predProbTstt=predict(cModel1, GermanDataTst, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst$RESPONSE)
mean(predTstt==GermanDataTst$RESPONSE)

#Q5
summary(rpmodelTrn2)


#Q6
#Profit curve for 30% testing data
PROFITVAL=100
COSTVAL=-500
scoreTst=predict(rpmodelTrn2,GermanDataTst_30, type="prob")[,'Good'] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, GermanDataTst_30$RESPONSE)
prLifts=prLifts[order(-scoreTst) ,] 
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`GermanDataTst_30$RESPONSE`=='Good', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))
write.xlsx(prLifts, "C:/Users/prath/Desktop/DM/prLifts_30.xlsx") 
plot(prLifts$cumProfits)

#find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))

CTHRESH=0.903
#Testing accuracy for rpart
predProbTstt=predict(rpmodelTrn2, GermanDataTst_30, type='prob')
predTstt = ifelse(predProbTstt[,"Good"] >= CTHRESH, 'Good', 'Bad')
ct = table( pred = predTstt, true=GermanDataTst_30$RESPONSE)
mean(predTstt==GermanDataTst_30$RESPONSE)

#Q7

#Renaming the CO-APPLICANT to CO_APPLICANT as R does not accept special characters.
colnames(GermanDataTrn_70)[9] <- "CO_APPLICANT"
colnames(GermanDataTst_30)[9] <- "CO_APPLICANT"
GermanDataTrn_70.imputed <- rfImpute(RESPONSE ~ ., data=GermanDataTrn_70)

#Random forest Model1 : 
rfModel = randomForest(RESPONSE ~ .,data=GermanDataTrn_70, ntree=200, importance=TRUE )

#Variable importance
a <- importance(rfModel)
varImpPlot(rfModel)

#ROC curve
perf_rfTst=performance(prediction(predict(rfModel,GermanDataTst_30, type="prob")[,2], GermanDataTst_30$RESPONSE), "tpr", "fpr")
plot(perf_rfTst)

#AUC value
aucPerf = performance(prediction(predict(rfModel,GermanDataTst_30, type="prob")[,2], GermanDataTst_30$RESPONSE), measure = "auc")
aucPerf@y.values


#Random Forest Model2:
rfModel1 = randomForest(RESPONSE ~ .,data=GermanDataTrn_70, ntree=300, importance=TRUE )

#Variable importance
a <- importance(rfModel1)
varImpPlot(rfModel1)

#ROC curve
perf_rfTst1=performance(prediction(predict(rfModel1,GermanDataTst_30, type="prob")[,2], GermanDataTst_30$RESPONSE), "tpr", "fpr")
plot(perf_rfTst1)

#AUC value
aucPerf = performance(prediction(predict(rfModel1,GermanDataTst_30, type="prob")[,2], GermanDataTst_30$RESPONSE), measure = "auc")
aucPerf@y.values

#Random Forest Model3:
rfModel2 = randomForest(RESPONSE ~ .,data=GermanDataTrn_70, ntree=400, importance=TRUE )

#Variable importance
a <- importance(rfModel2)
varImpPlot(rfModel2)

#ROC curve
perf_rfTst2=performance(prediction(predict(rfModel2,GermanDataTst_30, type="prob")[,2], GermanDataTst_30$RESPONSE), "tpr", "fpr")
plot(perf_rfTst2)

#AUC value
aucPerf = performance(prediction(predict(rfModel2,GermanDataTst_30, type="prob")[,2], GermanDataTst_30$RESPONSE), measure = "auc")
aucPerf@y.values

#Random Forest Model4:
rfModel3 = randomForest(RESPONSE ~ .,data=GermanDataTrn_70, ntree=500, importance=TRUE )

#Variable importance
a <- importance(rfModel3)
varImpPlot(rfModel3)

#ROC curve
perf_rfTst3=performance(prediction(predict(rfModel3,GermanDataTst_30, type="prob")[,2], GermanDataTst_30$RESPONSE), "tpr", "fpr")
plot(perf_rfTst3)

#AUC value
aucPerf = performance(prediction(predict(rfModel3,GermanDataTst_30, type="prob")[,2], GermanDataTst_30$RESPONSE), measure = "auc")
aucPerf@y.values

 

#Splitting of Data in 70:30 and obtaining ROC curve using Model with Gini Index , CP = 0.0085 , MinSplit = 8 and MaxDepth = 12
nr=nrow(GermanData)
trnIndex1 = sample(1:nr, size = round(0.7*nr), replace=FALSE)
GermanDataTrn1_70=GermanData[trnIndex1,]  
GermanDataTst1_30 = GermanData[-trnIndex1,] 


rpmodelTrn2 <- rpart(RESPONSE ~ ., data=GermanDataTrn1_70, method="class",parms = list(split ='gini'),control = rpart.control(cp=0.0085,minsplit = 8,maxdepth = 12))
rpart.plot::prp(rpmodelTrn2, type=2, extra=1)
summary(rpmodelTrn2)
plotcp(rpmodelTrn2)
predTrn2=predict(rpmodelTrn2, GermanDataTrn1_70, type='class')
table(pred = predTrn2, true = GermanDataTrn1_70$RESPONSE)
mean(predTrn2==GermanDataTrn1_70$RESPONSE)

predTst2=predict(rpmodelTrn2, GermanDataTst1_30, type='class')
table(pred = predTst2, true=GermanDataTst1_30$RESPONSE)
mean(predTst2==GermanDataTst1_30$RESPONSE)

#ROC curve
scoreTst1=predict(rpmodelTrn2,GermanDataTst1_30, type="prob")[,'Good']  
rocPredTst1 = prediction(scoreTst1, GermanDataTst1_30$RESPONSE, label.ordering = c('Bad', 'Good'))  
perfROCTst1=performance(rocPredTst1, "tpr", "fpr")
plot(perfROCTst1)

#AUC curve
accPerf = performance(rocPredTst1, measure = "acc")
plot(accPerf)
aucPerf = performance(rocPredTst1, measure = "auc")
aucPerf@y.values  

#Comparison between Random Forest best model and Decision Tree model with Gini Index , CP = 0.0085 , MinSplit = 8 and MaxDepth = 12
plot(perfROCTst1, col='red')
plot(perf_rfTst2, col='blue', add=TRUE)
legend('bottomright', c('DecisionTree', 'RandomForest'), lty=1, col=c('red', 'blue', 'green'))


#Profit curve for 30% testing data on Random Forest model
PROFITVAL=100
COSTVAL=-500
scoreTst=predict(rfModel2,GermanDataTst_30, type="prob")[,'Good'] 
prLifts=data.frame(scoreTst)
prLifts=cbind(prLifts, GermanDataTst_30$RESPONSE)
prLifts=prLifts[order(-scoreTst) ,] 
prLifts<-prLifts %>% mutate(profits=ifelse(prLifts$`GermanDataTst_30$RESPONSE`=='Good', PROFITVAL, COSTVAL), cumProfits=cumsum(profits))
write.xlsx(prLifts, "C:/Users/prath/Desktop/DM/prLifts_RandomForest_30.xlsx") 
plot(prLifts$cumProfits)

#find the score coresponding to the max profit
maxProfit= max(prLifts$cumProfits)
maxProfit_Ind = which.max(prLifts$cumProfits)
maxProfit_score = prLifts$scoreTst[maxProfit_Ind]
print(c(maxProfit = maxProfit, scoreTst = maxProfit_score))