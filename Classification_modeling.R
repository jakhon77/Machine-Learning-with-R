# Project II
# Preliminary Models


rm(list=ls())
# ---------------------------------------
wd = "C:/Users/jkhat/Downloads/Project2(2)/Project2" 
# this is the path to the unzipped folder on my computer; and you need to change it. 
setwd(wd)

source("P2-AUC.R")

# ---------------------------------------
library(sp)  # if it is not installed, install.packages("sp") 
library(rgeos)
library(gstat)
library(RColorBrewer)

# ---------------------------------------
# I combined the training data from 20 wafers into one big file
data = read.csv("Data_Train_Processed.csv", header = TRUE, stringsAsFactors = FALSE)
data = data[,-ncol(data)] # remove the last column which indicates the wafer ID
dim(data)

data.test= read.csv("Data_Test_Processed.csv", header = TRUE, stringsAsFactors = FALSE)
dim(data.test)
# # load the testing data: this is the testing data you need which are in a folder called data_test
# # each file contains the feature information of a wafer
data.test.1 = read.csv("data_test/W21_P.csv",header=TRUE,stringsAsFactors=FALSE)
data.test.2 = read.csv("data_test/W22_P.csv",header=TRUE,stringsAsFactors=FALSE)
data.test.3 = read.csv("data_test/W23_P.csv",header=TRUE,stringsAsFactors=FALSE)
data.test.4 = read.csv("data_test/W24_P.csv",header=TRUE,stringsAsFactors=FALSE)
data.test.5 = read.csv("data_test/W25_P.csv",header=TRUE,stringsAsFactors=FALSE)
# 
# data.test = rbind(data.test.1,data.test.2,data.test.3,data.test.4,data.test.5)
# write.csv(data.test,"Data_Test_Processed.csv",row.names = FALSE)
# # this is process testing data you may use 




# ---------------------------------------
# ---------------------------------------
# Data quality check and data cleaning
# ---------------------------------------
# ---------------------------------------

# Check the training data quality:

# calcuate the data missing rate for each column
miss.rate = array()
for (i in 1:ncol(data)){
  miss.rate[i] = sum(is.na(data[,i]))/nrow(data)
}
plot(miss.rate,type="h")
# many columns have no data at all, and should be excluded
case = which(miss.rate>0.5) # remove columns with missing rate > 0.5
data = data[,-case]
dim(data) # it turns out 132 columns can be excluded

data.test = data.test[,-(case-1)]
dim(data.test)

# accordingly, we remove those columns from the testing data sets
data.test.1 = data.test.1[,-(case-1)]
data.test.2 = data.test.2[,-(case-1)]
data.test.3 = data.test.3[,-(case-1)]
data.test.4 = data.test.4[,-(case-1)]
data.test.5 = data.test.5[,-(case-1)]

# ----------------

# calculate the columns with the same input (we need to remove those columns)
col.max = apply(data, 2, max, na.rm=TRUE) # calculate the maximum value for each column
col.min = apply(data, 2, min, na.rm=TRUE) # calculate the minimum value for each column
d = (col.max - col.min)
case = which(d==0) # select columns where the maximum = minimum (meaning that these columns contain only a contant value)
data = data[,-case]
dim(data) # it turns out another 34 columns have been excluded

data.test = data.test[,-(case-1)]
dim(data.test)

# accordingly, we remove those columns from the testing data sets
data.test.1 = data.test.1[,-(case-1)]
data.test.2 = data.test.2[,-(case-1)]
data.test.3 = data.test.3[,-(case-1)]
data.test.4 = data.test.4[,-(case-1)]
data.test.5 = data.test.5[,-(case-1)]

data.zones <- read.csv("W1_P.csv",header=TRUE,stringsAsFactors=FALSE)
distance = data.zones[,3]
letters = as.factor(data.zones[,4])

data$zone = rep(letters, times=20)
data$distance = rep(distance, times=20)

data$A = 0
case = which(data$zone=="A")
data$A[case] = 1

dim(data)

# data.test$zone = rep(letters, times=5)
# data.test$A = 0
# case = which(data.test$zone=="A")
# data.test$A[case] = 1


# NOW, we're ready to start the modeling
# NOW, we're ready to start the modeling
# NOW, we're ready to start the modeling


# ---------------------------------------
# ---------------------------------------
# Model 1: We always start with the simple model
# 
# Description: 
# 1) predict the failure probability based on locations only
# 2) for each location, we simply calculate the empirical (i.e., average) failure probabilities at that location
# 3) for each location, the calculated failure probabilities are directly used as predictions
# ---------------------------------------
# ---------------------------------------
xy = data[1:302,1:2]# get the coordinates of a wafer
m = matrix(data[,3],ncol=20) # get the fail/pass results for all 20 wafers
mean.p = rowMeans(m) #calculate the mean failure probabilities
model_1 = data.frame(cbind(xy, mean.p))
#generate a data.frame that contains the empirical failure probabilities at each location

# make predictions for each wafer; since model_1 uses only the location information,
# the predictions for all 5 wafers are the same
# model_1_output = cbind(model_1[,3],model_1[,3],model_1[,3],model_1[,3],model_1[,3])
# 
# #save the predicted results to the output folder
# write.table(model_1_output, "output/teamname_0501_a.csv", sep=",", row.names=FALSE, col.names=FALSE)
failure.prob = model_1[,3]
data$failure.prob = rep(failure.prob, times=20)
# ---------------------------------------
# ---------------------------------------
# Variable selection using Random Forests
# ---------------------------------------
# ---------------------------------------
library(randomForest)
data.rf = data # data.rf will be used for random forests
data.rf[,3] = as.factor(data.rf[,3])
# running the RF algorithm 1
fit.rf = randomForest(final_fail_bin_str~., data = data.rf,
                      ntree = 500, do.trace=100,na.action = na.omit,importance=TRUE)
im = importance(fit.rf)
# head(im)
# variable importance
varImpPlot(fit.rf,type=1); varImpPlot(fit.rf,type=2)

plot(sort(im[,4]), type="h")

# -------------------------------------
# -------------------------------------
# sorting the importance based on MeanDecreaseGini
im2 = im[order(-im[,4]),]
head(im2)
# selecting first n imprtant features
s=5
feature.s = rownames(im2)[c(1:s)]
case.f=array()
for(i in 1:s){
  case.f[i]=which(colnames(data)==feature.s[i])
}

# ---------------------------------------
# ---------------------------------------
# Model 2: Random Forests based on the s selected covariates
# ---------------------------------------
# ---------------------------------------
library(randomForest)

s=4
feature.s = rownames(im2)[c(1:s)]
case.f=array()
for(i in 1:s){
  case.f[i]=which(colnames(data)==feature.s[i])
}

data.rf = data[,c(3,1,2,case.f)]
data.rf[,1] = as.factor(data.rf[,1])
data.rf.b = data[,c(3,1,2,504,195,193)]
data.rf.b[,1] = as.factor(data.rf.b[,1])
# # running the RF algorithm
fit.rf = randomForest(final_fail_bin_str~., data = data.rf,
                      ntree = 500, do.trace=100, na.action = na.omit, confusion=TRUE)
fit.rf.b = randomForest(final_fail_bin_str~., data = data.rf.b,
                      ntree = 500, do.trace=100, na.action = na.omit, confusion=TRUE)

# generate predicted probabilities:
pred.1 = predict(fit.rf, data.test.1[,c(1,2,case.f-1)], type="prob",)[,2]
pred.2 = predict(fit.rf, data.test.2[,c(1,2,case.f-1)], type="prob")[,2]
pred.3 = predict(fit.rf.b, data.test.3[,c(1,2,503,194,192)], type="prob")[,2]
pred.4 = predict(fit.rf, data.test.4[,c(1,2,case.f-1)], type="prob")[,2]
pred.5 = predict(fit.rf.b, data.test.5[,c(1,2,503,194,192)], type="prob")[,2]
# # since model_2 uses only the location information, the predictions for all 5 wafers are the same
model_2_output = cbind(pred.1,pred.2,pred.3,pred.4,pred.5)
head(model_2_output)
write.table(model_2_output,"output/Matrix_2020_0502_n.csv",row.names=FALSE, sep=",",col.names=FALSE)

# Taking a closer look at model_2_output, 
# you'll see that the 3rd and 5th columns of model_2_output are zero
# This is because "layer_41_sigma_Min" are not valable for wafer 23 and 25 in the testing data set.
# Hence, for wafer 23 and 25, we need to build a separate model based on only 3 covariates
# fit.rf.b = randomForest(final_fail_bin_str~., data = data.rf[,c(1,2,3)],
#                       ntree = 500, do.trace=100, na.action = na.exclude, confusion=TRUE)
# pred.1 = predict(fit.rf.b, newdata= data.test.1[,c(1,2,c(case2, case3, case4)-1)], type="prob")[,2]
# pred.2 = predict(fit.rf.b, newdata=data.test.2[,c(1,2,c(case2, case3, case4)-1)], type="prob")[,2]
# pred.3 = predict(fit.rf.b, newdata=data.test.3[,c(1,2,c(case2, case3, case4)-1)], type="prob")[,2]
# pred.4 = predict(fit.rf.b, newdata=data.test.4[,c(1,2,c(case2, case3, case4)-1)], type="prob")[,2]
# pred.5 = predict(fit.rf.b, newdata=data.test.5[,c(1,2,c(case2, case3, case4)-1)], type="prob")[,2]
# # since model_2 uses only the location information, the predictions for all 5 wafers are the same
# model_2_output = cbind(pred.1,pred.2,
#                        pred.3,pred.4,pred.5)
# now, we have the complete predicted probabilities
#write.table(model_2_output,"output/teamname_0427_2.csv",row.names=FALSE, sep=",",col.names=FALSE)


# ---------------------------------------
# ---------------------------------------
# Model 3: Perform classification using xgboost
# ---------------------------------------
# ---------------------------------------
library(xgboost)

s=5
feature.s = rownames(im2)[c(1:s)]
case.f=array()
for(i in 1:s){
  case.f[i]=which(colnames(data)==feature.s[i])
}
# locate the 4 selected variables:
# case1 = which(colnames(data) == "layer_41_sigma_Min")
# case2 = which(colnames(data) == "layer_43_sigma_Min")
# case3 = which(colnames(data) == "layer_51_sigma_Min")
# case4 = which(colnames(data) == "layer_55_sigma_Min")
#data.boost = data[,c(3, 1,2, case2, case3, case4)] # data.boost will be used for XGBoost

data.boost = data[,c(3,1,2,case.f)]
# running the xgboost algorithm (with some arbitrary model inputs)
setting <- list(max_depth = 5, eta = 0.003, nthread = 4, 
                objective="binary:logistic")
fit.boost = xgboost(data = as.matrix(data.boost[,2:8]), 
                      label = data.boost$final_fail_bin_str,
                      nrounds = 325,
                      params = setting,verbose=0)
fit.boost.b = xgboost(data = as.matrix(data.boost[,c(2:6,8)]), 
                    label = data.boost$final_fail_bin_str,
                    nrounds = 325,
                    params = setting,verbose=0)
# predicts the probabilities
# pred = predict(fit.boost, as.matrix(data.test[,c(1,2,501:504,case.f-1)]))
# pred.final = matrix(pred,ncol=5)

pred.1 = predict(fit.boost, as.matrix(data.test.1[,c(1,2,case.f-1)]))
pred.2 = predict(fit.boost, as.matrix(data.test.2[,c(1,2,case.f-1)]))
pred.3 = predict(fit.boost.b, as.matrix(data.test.3[,c(1,2,503,194,192,189)]))
pred.4 = predict(fit.boost, as.matrix(data.test.4[,c(1,2,case.f-1)]))
pred.5 = predict(fit.boost.b, as.matrix(data.test.5[,c(1,2,503,194,192,189)]))

model_3_output = cbind(pred.1,pred.2,pred.3,pred.4,pred.5)
head(model_3_output)
write.table(model_3_output,"output/Matrix_2020_0503_p.csv",row.names=FALSE, sep=",",col.names=FALSE)

# # ---------------------------------------
# # ---------------------------------------
# # Model 4: Perform Logistic Regression
# # ---------------------------------------
# # ---------------------------------------
# # locate the 4 selected variables:
# case1 = which(colnames(data) == "layer_41_sigma_Min")
# case2 = which(colnames(data) == "layer_43_sigma_Min")
# case3 = which(colnames(data) == "layer_51_sigma_Min")
# case4 = which(colnames(data) == "layer_55_sigma_Min")
# data.lr = data[,c(3, case2, case3, case4)] # data.lr will be used for the logistics regression
# data.lr[,1] = as.factor(data.lr[,1])
# 
# # running the logistic regression
# fit.lr = glm(final_fail_bin_str~., data = data.lr[,c(1,2,3,4)], family=binomial)
# # predicts the probabilities
# pred.1 = predict(fit.lr, data.test.1[,c(case1, case2, case3, case4)-1], type="response")
# pred.2 = predict(fit.lr, data.test.2[,c(case1, case2, case3, case4)-1], type="response")
# pred.3 = predict(fit.lr, data.test.3[,c(case1, case2, case3, case4)-1], type="response")
# pred.4 = predict(fit.lr, data.test.4[,c(case1, case2, case3, case4)-1], type="response")
# pred.5 = predict(fit.lr, data.test.5[,c(case1, case2, case3, case4)-1], type="response")
# model_4_output = cbind(pred.1,pred.2,
#                        pred.3,pred.4,pred.5)





# ---------------------------------------
# ---------------------------------------
# Cross-validation-based model selection
# Boosting trees requires us to find appropriate values for the 3 tuning parameters; 
# This can be done through cross-validation
# ---------------------------------------
# ---------------------------------------

library(xgboost)
source("P2-AUC.R") # the code used to calcuate AUC

# locate the 4 selected variables:
# case1 = which(colnames(data) == "layer_41_sigma_Min")
# case2 = which(colnames(data) == "layer_43_sigma_Min")
# case3 = which(colnames(data) == "layer_51_sigma_Min")
# case4 = which(colnames(data) == "layer_55_sigma_Min")
#data.boost = data[,c(3, 1,2, case2, case3, case4)] # data.boost will be used for XGboost
s=5
feature.s = rownames(im2)[c(1:s)]
case.f=array()
for(i in 1:s){
  case.f[i]=which(colnames(data)==feature.s[i])
}
data.boost = data[,c(3,1,2,case.f)]
# Since we're going to do a cross-validation, there is a need to separate the data into two parts:
# Part 1: training set
set.seed(10)
case = sample(c(1:nrow(data.boost)), 4500) # for example, 4500 rows are randomly selected as training data set
data.train = data.boost[case,]
# Part 2: testing set
data.test = data.boost[-case,] # testing data set


# candidate tree complexity, d
d.set = c(4,5)
# candidate learning rate, eta
eta.set =seq(0.007,0.01,0.001)
# candidate number of trees
B.set = seq(275,325,10) 
n.combination = length(d.set)*length(eta.set)*length(B.set)
 
output = array(0/0, dim=c(n.combination, 4))
output = data.frame(output)
colnames(output) = c("AUC", "d", "eta", "B")
jj = 1
for (i in 1:length(d.set)){
  for (j in 1:length(eta.set)){
    for (q in 1:length(B.set)){
      d = d.set[i]
      eta = eta.set[j] 
      B = B.set[q]
      
      # running the xgboost algorithm
      setting <- list(max_depth = d, eta = eta, nthread = 4, 
                      objective="binary:logistic")
      fit.boost = xgboost(data = as.matrix(data.train[,2:10]), 
                          label = data.train$final_fail_bin_str,
                          nrounds = B,
                          params = setting,verbose=0)
      
      # predicts the probabilities
      pred = predict(fit.boost, as.matrix(data.test[,-1]))

      # calculate AUC (area under the curve)
      # get the true observation
      obs = data.test[,1]
      
      # ROC curve:
      TP = FP = array()
      ii = 1
      for (thres in seq(0,1,0.001)){
        pred.c = rep(0, length(pred)) # set all prediction to 0
        pred.c[pred>thres] = 1 
        TP[ii] = length(which( obs + pred.c== 2 ))/length(which(obs==1))
        FP[ii] = length(which( obs - pred.c==-1 ))/length(which(obs==0))
        ii = ii+1
      }
      plot(FP, TP, type="l",xlab="FALSE POSITIVE", ylab="TRUE POSITIVE")
      AUC = auc(TP,FP) # calcuate the AUC using the user-defined function, auc
      
      # update output
      output[jj,1] = AUC
      output[jj,2] = d
      output[jj,3] = eta
      output[jj,4] = B
      jj=jj + 1
      print(jj)
    }
  }
}

head(output)
plot(output[,1],type="h")

# find the top 10 combinations which give us high AUC
output.order = output[order(output[,1], decreasing = TRUE),]
head(output.order,10)



# Logistic regression using GAM

library(gam)
s=6
feature.s = rownames(im2)[c(1:s)]
case.f=array()
for(i in 1:s){
  case.f[i]=which(colnames(data)==feature.s[i])
}
data.gam = data[,c(3,1,2,case.f)]



lr.gam<-gam(final_fail_bin_str~s(x,df=3)+s(y,df=2)+s(failure.prob,df=2)+s(layer_55_sigma_Min,df=4)+s(layer_51_sigma_Min,df=3),
            data = data.train.gam ,family=binomial)


lr.gam.b<-gam(final_fail_bin_str~s(x,df=3)+s(y,df=2)+s(failure.prob,df=2)+s(layer_55_sigma_Min,df=4)+s(layer_51_sigma_Min,df=4),
            data = data.train.gam ,family=binomial)

# predicts the probability that an e-mail will be spam
pred.1 = predict(lr.gam, data.test.1, type="response")
pred.2 = predict(lr.gam, data.test.2, type="response")
pred.3 = predict(lr.gam.b, data.test.3, type="response")
pred.4 = predict(lr.gam, data.test.4, type="response")
pred.5 = predict(lr.gam.b, data.test.5, type="response")

model_4_output = cbind(pred.1,pred.2,pred.3,pred.4,pred.5)
head(model_4_output)
write.table(model_4_output,"output/Matrix_2020_0503_k.csv",row.names=FALSE, sep=",",col.names=FALSE)
