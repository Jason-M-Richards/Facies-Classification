##Rewriting the Fcies Classification in R rather than Python

setwd("P:/DataScience/WellLogs - DSDP Project")

Traindata<-read.csv("training_data_clean.csv")

#convert Facies to factor rather than number
Traindata$Facies<-as.factor(Traindata$Facies)

#remove one well and reserve it for testing
data<-subset(Traindata, WellName != "SHANKLE")
test_well<-subset(Traindata, WellName=="SHANKLE")

attach(data)
library(e1071)
library(ggplot2)


#Comment out- not appropriate here - need cole's method for choosing the test/train split
#identify label & features we want to use
#col<-c("Facies","GR","ILD_log10","DeltaPHI","PHIND","PE","NM_M","RELPOS")
#s<-sample(2783, 1500) #out of 2783 records, randomly choose 1500
#data_Train<-data[s,col]
#data_Test<-data[-s,col]

#fit the model to data
svmfit <- svm(Facies ~ .-WellName -Formation -Depth, data=data,  cost=500) 

print(svmfit)

#identify a better cost parameter
#tuned<-tune(svm, Facies  ~ ILD_log10 +GR +DeltaPHI +PHIND +RELPOS , data=data,  ranges=list(cost=c(0.001, 0.01, .1,1,10,100, 300,500)))
#summary(tuned)
#confirmed best cost parameter is 100. Comment out last 2 lines of code and update svmfit to use cost=500.


##############predict#################

#now predict
Prediction<-predict(svmfit, test_well, type="class")


###########confusion matrix#######################
cm<-table(test_well[,1],Prediction)
comparetable<-cbind(test_well, Prediction)



#export data to visualize in Spotfire
write.csv(cm, file="RCM.csv")
write.csv(comparetable, file="RewriteInROutput.csv")

##############evaluate model performance#####################

#library(ROCR)
#performance(ROCRpred, "f") #f-score
#RP.perf<-performance(comparetable$Facies, comparetable$Prediction, "prec","rec")
#scores<-svmfit

###############decision tree###################

library(rpart)
library(rpart.plot)


fit <- rpart(Facies ~ .-WellName -Depth -Formation , method="class", data=data)

#printcp(fit) #display results
#plotcp(fit) #visualize cross-validation results
#summary(fit) #detailed summary of splits

#plot(fit, uniform=TRUE)
#text(fit, use.n=TRUE,  cex=.8)

Prediction<-predict(fit, test_well, type="class")


dtcm<-table(test_well[,1],Prediction)
comparetabledt<-cbind(test_well, Prediction)

write.csv(dtcm, file="RDTCM.csv")
write.csv(comparetabledt, file="RDTCompareTable.csv")

###############random forest###################

library(randomForest)



rffit <- randomForest(Facies ~ .-WellName -Formation -Depth,  data=data)

#printcp(rffit) #display results
#plotcp(rffit) #visualize cross-validation results
#summary(rffit) #detailed summary of splits

#plot(rffit, uniform=TRUE)
#text(rffit, use.n=TRUE,  cex=.8)

Prediction<-predict(rffit, test_well, type="class")


rfcm<-table(test_well[,1],Prediction)
comparetablerf<-cbind(test_well, Prediction)

write.csv(rfcm, file="RRFCM.csv")
write.csv(comparetablerf, file="RRFCompareTable.csv")

#print(importance(RFPrediction

################Feature Selection###################
library(Boruta)
Features<-Boruta(Facies ~ .-Formation -WellName -NM_M,  data=data, doTrace=2)
print(Features)
plot(Features)

