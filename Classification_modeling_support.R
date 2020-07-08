

output = array(0/0, dim=c(100, 2))
output = data.frame(output)
colnames(output) = c("s", "AUC")
s=5
for (i in 1:50){
  feature.s = rownames(im2)[c(1:s)]
  case.f=array()
  for(i in 1:s){
    case.f[i]=which(colnames(data)==feature.s[i])
  }
  
  # ---------------------------------------
  # ---------------------------------------
  # Model 2: Random Forests based on the 4 selected covariates
  # ---------------------------------------
  # ---------------------------------------
  library(randomForest)
  data.rf = data[,c(3,1,2,case.f)]
  data.rf[,1] = as.factor(data.rf[,1])
  case = sample(c(1:nrow(data.rf)), 4500)
  data.train = data.rf[case,]
  # testing set
  data.test = data.rf[-case,] 
  obs = data.test$final_fail_bin_str
  
  # # running the RF algorithm
  fit.rf = randomForest(final_fail_bin_str~., data = data.train,
                        ntree = 500, do.trace=100, na.action = na.omit, confusion=TRUE)
  
  pred.rf = predict(fit.rf, data.test, type="prob")[,2]
  
  
  # ROC curve:
  TP = FP = array()
  i = 1
  for (thres in seq(0,1,0.01)){
    pred.rf.c = rep(0, nrow(data.test)) # set all prediction to 0
    pred.rf.c[pred.rf>thres] = 1 
    TP[i] = length(which( obs + pred.rf.c==2 ))/length(which(obs==1))
    FP[i] = length(which( obs - pred.rf.c==-1 ))/length(which(obs==0))
    i = i+1
  }
  AUC.rf = round(auc(TP,FP),3)
  output[s,1] = s
  output[s,2] = AUC.rf
  s = s+2
  print(s)
}

head(output)
plot(output[,1],type="h")
output.order = output[order(output[,2], decreasing = TRUE),]
head(output.order,10)








data.test.1$zone = letters
data.test.1$distance = distance
data.test.1$A = 0
case = which(data.test.1$zone=="A")
data.test.1$A[case] = 1
data.test.1$failure.prob = failure.prob

data.test.2$zone = letters
data.test.2$distance = distance
data.test.2$A = 0
case = which(data.test.2$zone=="A")
data.test.2$A[case] = 1
data.test.2$failure.prob = failure.prob

data.test.3$zone = letters
data.test.3$distance = distance
data.test.3$A = 0
case = which(data.test.3$zone=="A")
data.test.3$A[case] = 1
data.test.3$failure.prob = failure.prob

data.test.4$zone = letters
data.test.4$distance = distance
data.test.4$A = 0
case = which(data.test.4$zone=="A")
data.test.4$A[case] = 1
data.test.4$failure.prob = failure.prob

data.test.5$zone = letters
data.test.5$distance = distance
data.test.5$A = 0
case = which(data.test.5$zone=="A")
data.test.5$A[case] = 1
data.test.5$failure.prob = failure.prob






s=1
feature.s = rownames(im2)[c(1:s)]
case.f=array()
for(i in 1:s){
  case.f[i]=which(colnames(data)==feature.s[i])
}

library(randomForest)
data.rf = data[,c(3,1,2,502,503,case.f)]
data.rf[,1] = as.factor(data.rf[,1])

set.seed(10)
case.select = sample(1:nrow(data.rf),4500)
data.rf.train = data.rf[case.select,]
data.rf.test = data.rf[-case.select,]
obs = data.rf.test$final_fail_bin_str

fit.rf = randomForest(final_fail_bin_str~., data = data.rf.train,
                      ntree = 500, do.trace=100, na.action = na.omit, confusion=TRUE)

#plot(c(1:300), fit.rf$err.rate,col="darkgreen",xlab="# trees grown", ylab="mse")
pred.rf = predict(fit.rf, data.rf.test, type="prob",)[,2]
# ROC curve:
TP = FP = array()
i = 1
for (thres in seq(0,1,0.01)){
  pred.rf.c = rep(0, nrow(data.rf.test)) # set all prediction to 0
  pred.rf.c[pred.rf>thres] = 1 
  TP[i] = length(which( obs + pred.rf.c==2 ))/length(which(obs==1))
  FP[i] = length(which( obs - pred.rf.c==-1 ))/length(which(obs==0))
  i = i+1
}
plot(FP, TP, type="l",xlab="FALSE POSITIVE", ylab="TRUE POSITIVE",lwd=2)
AUC.rf = round(auc(TP,FP),3)
AUC.rf









# Logistic regression using GAM

library(gam)
s=7
feature.s = rownames(im2)[c(1:s)]
case.f=array()
for(i in 1:s){
  case.f[i]=which(colnames(data)==feature.s[i])
}
set.seed(100)
case.gam = sample(1:nrow(data),4500)
data.train.gam = data[case,] # select 3000 data points for training
data.test.gam = data[-case,]
obs = data.test.gam$final_fail_bin_str

# lr.gam<-gam(final_fail_bin_str~s(x,df=3)+s(y,df=2)+s(distance,df=2)+s(failure.prob,df=2)+s(layer_55_sigma_Min,df=4)+s(layer_51_sigma_Min,df=3)
#             +s(layer_41_sigma_Min,df=4)+s(layer_43_sigma_Min,df=3)+s(WIS_layer_55_Standard_DieSubMargin_Gray_sigma,df=2),
#             data = data.train.gam ,family=binomial)

lr.gam<-gam(final_fail_bin_str~s(x,df=3)+s(y,df=2)+s(distance,df=3)+s(failure.prob,df=2)+s(layer_55_sigma_Min,df=4)+s(layer_51_sigma_Min,df=4)
            +s(layer_43_sigma_Min,df=5)+s(WIS_layer_55_Standard_DieSubMargin_Gray_sigma,df=2)+s(layer_35_mean_Min,df=2),
            data = data.train.gam ,family=binomial)

# predicts the probability that an e-mail will be spam
pred.lr.gam = predict(lr.gam, data.test.gam, type="response")
# ROC curve:
TP = FP = array()
i = 1
for (thres in seq(0,1,0.01)){
  pred.lr.c = rep(0, nrow(data.test.gam)) # set all prediction to 0
  pred.lr.c[pred.lr.gam>thres] = 1 
  TP[i] = length(which( obs + pred.lr.c==2 ))/length(which(obs==1))
  FP[i] = length(which( obs - pred.lr.c==-1 ))/length(which(obs==0))
  i = i+1
}
par(mfrow=c(1,3)) #to partition the Plotting Window
plot(lr.gam,se = TRUE) 
#plot(FP, TP, type="l",xlab="FALSE POSITIVE", ylab="TRUE POSITIVE",lwd=2,col="red")
AUC.lr.gam = round(auc(TP,FP),3)
AUC.lr.gam


