# Lab Session
# Some preliminary models for Case Study I


setwd("C:/Users/jkhat/Downloads/Project_1_stat")
# this where you save the data: data3.RData
load("data3.RData")

data3=na.omit(data3)

#case1 = which(is.na(data3$w_x))
#case2 = which(is.na(data3$w_y))

#data3$w_x = (data3$w_x - mean(data3$w_x[-case1]))/sqrt(var(data3$w_x[-case1]))
#data3$w_y = (data3$w_y - mean(data3$w_y[-case2]))/sqrt(var(data3$w_y[-case2])) 

library(openair)
library(glmnet)
library(pls)

for (i in 3:12){
  data3[,i] = (data3[,i] - mean(data3[,i]))/sqrt(var(data3[,i]))
} 

for (i in 51:52){
  data3[,i] = (data3[,i] - mean(data3[,i]))/sqrt(var(data3[,i]))
} 
#data3$w_x = scale(data3$w_x, center =T, scale = T) # alternative for standardization


# create two data sets: one for training and one for testing
time.cut = as.POSIXlt("2011-10-01 01:00:00", tz="GMT", format="%Y-%m-%d %H:%M:%S")

case = which(data3$date<time.cut)
data.train = data3[case,]
data.test = data3[-case,]
obs = data.test$o3
data.test = data.test[,-1]

# save the created testing data set and training data set
write.csv(data.train, "data_train.csv", row.names = FALSE)
write.csv(data.test, "data_test.csv", row.names = FALSE)



model.output = list() # create a list to store predictions from different models

source("model_sub.R") 
# model_sub.R is a sub-routine where different models are pre-defined
# refer to the lecture slides for model descriptions

# -----------------------------------
# pred = model_1(data.train, data.test)
# output = data.frame(cbind(pred, obs))
# colnames(output) = c("pred","obs")
# output$model = "M1:Baseline"
# model.output[[1]] = output

# -----------------------------------
modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")
TaylorDiagram(modData, obs = "obs", mod = "pred", group = "model",pch=20,cex=2)
conditionalQuantile(modData, obs = "obs", mod = "pred", type="model")


# -----------------------------------
# pred = model_2(data.train, data.test)
# output = data.frame(cbind(pred, obs))
# colnames(output) = c("pred","obs")
# output$model = "M2:Linear_Full"
# model.output[[2]] = output
# -----------------------------------


# -----------------------------------
pred = model_3(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M3:Linear_AIC_both"
model.output[[3]] = output
# -----------------------------------

# -----------------------------------
pred = model_3_for(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M3:Linear_AIC_for"
model.output[[3]] = output
# -----------------------------------

# -----------------------------------
pred = model_3_for(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M3:Linear_AIC_both"
model.output[[3]] = output
# -----------------------------------

# -----------------------------------
pred = model_4(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M4:PCR"
model.output[[10]] = output
# -----------------------------------

# -----------------------------------
pred = model_5(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M5:Lasso"
model.output[[1]] = output
# -----------------------------------

# -----------------------------------
pred = model_5.min(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M5.min:Lasso"
model.output[[1]] = output
# -----------------------------------
# -----------------------------------
pred = model_6(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M6:Ridge"
model.output[[2]] = output
# -----------------------------------

# -----------------------------------
pred = model_7(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M7:PLS"
model.output[[7]] = output
# -----------------------------------
modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")
TaylorDiagram(modData, obs = "obs", mod = "pred", group = "model",pch=20,cex=2)
conditionalQuantile(modData, obs = "obs", mod = "pred", type="model")


# -----------------------------------
pred = model_8(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M8:Linear_Full"
model.output[[8]] = output
# -----------------------------------

# -----------------------------------
pred = model_9(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M9:Linear_AIC"
model.output[[9]] = output
# -----------------------------------

# -----------------------------------
pred = model_10(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M10:Ridge"
model.output[[10]] = output
# -----------------------------------

# -----------------------------------
pred = model_11(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M11:Lasso"
model.output[[11]] = output
# -----------------------------------

# -----------------------------------
pred = model_12(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M12:PLS"
model.output[[12]] = output
# -----------------------------------
modData = do.call(rbind,model.output[c(3,6,7,8:12)])
modStats(modData, obs = "obs", mod = "pred", type = "model")
TaylorDiagram(modData, obs = "obs", mod = "pred", group = "model",pch=20,cex=2)
conditionalQuantile(modData, obs = "obs", mod = "pred", type="model")






# -----------------------------------
pred = model_13(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M13:Linear_AIC"
model.output[[13]] = output
# -----------------------------------

# -----------------------------------
pred = model_14(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M14:Ridge"
model.output[[14]] = output
# -----------------------------------

# -----------------------------------
pred = model_15(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M15:Lasso"
model.output[[15]] = output
# -----------------------------------
modData = do.call(rbind,model.output[c(8:11,13:15)])
modStats(modData, obs = "obs", mod = "pred", type = "model")
TaylorDiagram(modData, obs = "obs", mod = "pred", group = "model",pch=20,cex=2)
conditionalQuantile(modData, obs = "obs", mod = "pred", type="model")

# -----------------------------------
pred = model_16(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M16:Lasso"
model.output[[16]] = output
# -----------------------------------
modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")
TaylorDiagram(modData, obs = "obs", mod = "pred", group = "model",pch=20,cex=2)
conditionalQuantile(modData, obs = "obs", mod = "pred", type="model")





# ----------------
# Suppose Model 16 is selected
# Load the "data_pred.csv" for prediction
data.pred = read.csv("data_pred.csv", header = TRUE, stringsAsFactors=FALSE)

for (i in 2:11){
  data.pred[,i] = (data.pred[,i] - mean(data.pred[,i]))/sqrt(var(data.pred[,i]))
}
for (i in 51:52){
  data.pred[,i] = (data.pred[,i] - mean(data.pred[,i]))/sqrt(var(data.pred[,i]))
}
# Process output, and save it to a csv file to be submitted
pred = model_12(data3, data.pred)
write.table(pred, "InduZtrial_0326_n.csv", row.names=FALSE, col.names=FALSE)















