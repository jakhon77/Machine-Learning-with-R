# -------------------------------------
# -------------------------------------
# Model 1:
model_1 = function(data.train, data.test){
  data.tmp = data.train[, c("o3","date","site")]
  data.tmp$hour = as.numeric(substr(data.tmp$date, 12,13))
  data.tmp$site = as.numeric(substr(data.tmp$site, 2,2))
  data.agg = array(0/0, dim=c(24,7))
  for (i in 1:24){
    for (j in 1:7){
      case = which( (data.tmp$site == j)&(data.tmp$hour==(i-1)) )
      data.agg[i,j] = mean(data.tmp$o3[case],na.rm=TRUE)
    }
  }

  data.tmp = data.test[, c("date","site")]
  data.tmp$hour = as.numeric(substr(data.tmp$date, 12,13))
  data.tmp$site = as.numeric(substr(data.tmp$site, 2,2))

  pred = array(0/0, dim=c(nrow(data.tmp), 1))
  for (j in 1:nrow(data.tmp)){
    pred[j,1]=data.agg[data.tmp$hour[j]+1, data.tmp$site[j]]
  }  
  return(pred)  
}
# -------------------------------------
# -------------------------------------


# -------------------------------------
# -------------------------------------
# Model 2:
model_2 = function(data.train, data.test){
  data.tmp = data.train[, c(1,3,6:12,14:17,19,24,25,33:36,39,42:44,51:53,55:58)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  fit = lm(o3~., data.tmp)
  pred = predict(fit, data.test[, c(2,5:11,13:16,18,23,24,32:35,38,41:43,50:52,54:57)])
  pred = pred^2
  return(pred)  
}

pred = model_2(data.train, data.test) 
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M2: full"
model.output[[5]] = output

modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")
# -------------------------------------
# -------------------------------------

# -------------------------------------
# -------------------------------------
# Model 3:   Modifying the model using all the features                          
model_3 = function(data.train, data.test){
  data.tmp = data.train[, c(1,3,6:12,26:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  full = lm(o3~., data.tmp)
  null = lm(o3~1, data.tmp)
  fit = step(full,  scope = list(lower=null,upper=full), direction="both")
  pred = predict(fit, data.test[, c(2,5:11,25:58)])
  pred = pred^2
  return(pred)  
}

pred = model_3(data.train, data.test) 
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M3: aic_both"
model.output[[2]] = output

modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")

model_3_for = function(data.train, data.test){
  data.tmp = na.omit(data.train[, c(1,3,6:12,14:59)])
  data.tmp$o3 = na.omit(sqrt(data.tmp$o3))
  full = lm(o3~., data.tmp)
  null = lm(o3~1, data.tmp)
  fit = step(null,  scope = list(lower=null,upper=full), direction="forward")
  pred = predict(fit, data.test[, c(2,5:11,13:58)])
  pred = pred^2
  return(pred)  
}

model_3_for = function(data.train, data.test){
  data.tmp = na.omit(data.train[, c(1,3,6:12,14:59)])
  data.tmp$o3 = na.omit(sqrt(data.tmp$o3))
  full = lm(o3~., data.tmp)
  null = lm(o3~1, data.tmp)
  fit = step(full,  scope = list(lower=null,upper=full), direction="both")
  pred = predict(fit, data.test[, c(2,5:11,13:58)])
  pred = pred^2
  return(pred)  
}
# -------------------------------------
# -------------------------------------

# -------------------------------------
# -------------------------------------
# Model 4:
model_4 = function(data.train, data.test){
  data.tmp = data.train[, c(1,3,6:12,26:50,53:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  data.tmp.2 = data.test[, c(2,5:11,25:49,52:58)]
  
  X.mat.1 = as.matrix(data.tmp[,2:ncol(data.tmp)])
  X.mat.2 = as.matrix(data.tmp.2[,1:ncol(data.tmp.2)])
  X.mat = rbind(X.mat.1, X.mat.2)
  pr.out = prcomp(X.mat, scale=FALSE)
  y.mat = as.vector(data.tmp$o3)
  plot(pr.out)
  Z = data.frame( pr.out$x[,1:10]) # keep the first M PC
  data.train.new = Z[1:nrow(X.mat.1),]
  data.train.new$o3 = y.mat
  data.test.new = Z[(nrow(X.mat.1)+1):nrow(X.mat),]
  
  fit = lm(o3~., data.train.new)
  pred = predict(fit, data.test.new)
  pred = pred^2
  return(pred) 
}
# -------------------------------------
# -------------------------------------

# -------------------------------------
# -------------------------------------
library(glmnet)
# Model 5:
model_5 = function(data.train, data.test){
  data.tmp = data.train[, c(1,3,6:12,14:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)

  X.mat = as.matrix(data.tmp[,2:ncol(data.tmp)])
  y.mat = as.vector(data.tmp$o3)
  
  case1 = which(is.na(rowSums(X.mat)))
  case2 = which(is.na(y.mat))
  case = unique(c(case1, case2))

  fit = cv.glmnet(x=X.mat, y=y.mat, type.measure = "mse", alpha=1, family="gaussian")
  plot(fit)
  pred = predict(fit, as.matrix(data.test[, c(2,5:11,13:58)]), s=fit$lambda.1se)
  #pred = pred[, round(1+ncol(pred)/2)]
  pred = pred^2
  return(pred)  
}

model_5.min = function(data.train, data.test){
  data.tmp = data.train[, c(1,3,6:12,14:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  
  X.mat = as.matrix(data.tmp[,2:ncol(data.tmp)])
  y.mat = as.vector(data.tmp$o3)
  
  case1 = which(is.na(rowSums(X.mat)))
  case2 = which(is.na(y.mat))
  case = unique(c(case1, case2))
  
  fit = cv.glmnet(x=X.mat[-case,], y=y.mat[-case], type.measure = "mse", alpha=1, family="gaussian")
  pred = predict(fit, as.matrix(data.test[, c(2,5:11,13:58)]), s=fit$lambda.min)
  #pred = pred[, round(1+ncol(pred)/2)]
  pred = pred^2
  return(pred)  
}
# -------------------------------------
# -------------------------------------

 # -------------------------------------
# -------------------------------------
library(glmnet)
# Model 6:
model_6 = function(data.train, data.test){
  data.tmp = data.train[, c(1,3,6:12,14:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  
  X.mat = as.matrix(data.tmp[,2:ncol(data.tmp)])
  y.mat = as.vector(data.tmp$o3)
  
  case1 = which(is.na(rowSums(X.mat)))
  case2 = which(is.na(y.mat))
  case = unique(c(case1, case2))
  
  fit = cv.glmnet(x=X.mat[-case,], y=y.mat[-case], type.measure = "mse", alpha=0, family="gaussian")
  plot(fit)
  pred = predict(fit, as.matrix(data.test[, c(2,5:11,13:58)]), s=fit$lambda.min)
  #pred = pred[, round(1+ncol(pred)/2)]
  pred = pred^2
  return(pred)   
}
# -------------------------------------
# -------------------------------------
 
# -------------------------------------
# -------------------------------------
# Model 8:
model_8 = function(data.train, data.test){
  data.tmp = data.train[, c(1, 3, 11:12, 26:49, 51:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  fit = lm(o3~., data.tmp)
  pred = predict(fit, data.test[, c(3, 11:12, 26:49, 51:59)-1])
  pred = pred^2
  return(pred)  
}
# -------------------------------------
# -------------------------------------

# -------------------------------------
# -------------------------------------
# Model 9:
model_9 = function(data.train, data.test){
  data.tmp = data.train[, c(1, 3, 11:12, 26:49, 51:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  full = lm(o3~., data.tmp)
  null = lm(o3~1, data.tmp)
  fit = step(full,  scope = list(lower=null,upper=full), direction="backward")
  pred = predict(fit, data.test[, c(3, 11:12, 26:49, 51:59)-1])
  pred = pred^2
  return(pred)  
}
# -------------------------------------
# -------------------------------------

# -------------------------------------
# -------------------------------------
library(glmnet)
# Model 10:
model_10 = function(data.train, data.test){
  data.tmp = data.train[, c(1, 3, 11:12, 26:49, 51:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)

  X.mat = as.matrix(data.tmp[,2:ncol(data.tmp)])
  y.mat = as.vector(data.tmp$o3)
  
  case1 = which(is.na(rowSums(X.mat)))
  case2 = which(is.na(y.mat))
  case = unique(c(case1, case2))

  fit = glmnet(x=X.mat[-case,], y=y.mat[-case], lambda = seq(0,1,0.1), 
		alpha=0, family="gaussian")
  pred = predict(fit, as.matrix(data.test[, c(3, 11:12, 26:49, 51:59)-1]))
  pred = pred[, round(1+ncol(pred)/2)]
  pred = pred^2
  return(pred)  
}
# -------------------------------------
# -------------------------------------

 # -------------------------------------
# -------------------------------------
library(glmnet)
# Model 11:
model_11 = function(data.train, data.test){
  data.tmp = data.train[, c(1, 3, 11:12, 26:49, 51:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)

  X.mat = as.matrix(data.tmp[,2:ncol(data.tmp)])
  y.mat = as.vector(data.tmp$o3)
  
  case1 = which(is.na(rowSums(X.mat)))
  case2 = which(is.na(y.mat))
  case = unique(c(case1, case2))

  fit = glmnet(x=X.mat[-case,], y=y.mat[-case], 
		alpha=1, family="gaussian")
  pred = predict(fit, as.matrix(data.test[, c(3, 11:12, 26:49, 51:59)-1]))
  pred = pred[, round(1+ncol(pred)/2)]
  pred = pred^2
  return(pred)  
}
# -------------------------------------
# -------------------------------------
 
 # -------------------------------------
# -------------------------------------
pls.fit = plsr(o3~., data=data.tmp, scale = F)#, validation = "CV")
pred = predict(pls.fit, data.tmp.2,ncomp=33)


library(pls)
# Model 12:
model_12 = function(data.train, data.test){
  data.tmp = data.train[, c(1, 3,26:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)

  fit = plsr(o3~., data=data.tmp, scale=F, validation = "CV" )
  pred = predict(fit, data.test[, c(1, 3,26:59)-1],ncomp=7)
  pred = pred^2
  return(pred)  
}
pred = model_12(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M12: plsssss"
model.output[[3]] = output

modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")
# -------------------------------------
# -------------------------------------
 


# -------------------------------------
# -------------------------------------
# Model 13:
model_13 = function(data.train, data.test){
  data.tmp = data.train[, c(1, 3, 11:12, 14:25, 26:49, 51:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  full = lm(o3~., data.tmp)
  null = lm(o3~1, data.tmp)
  fit = step(full,  scope = list(lower=null,upper=full), direction="backward")
  pred = predict(fit, data.test[, c(3, 11:12, 14:25, 26:49, 51:59)-1])
  pred = pred^2
  return(pred)  
}
# -------------------------------------
# -------------------------------------

# -------------------------------------
# -------------------------------------
library(glmnet)
# Model 14:
model_14 = function(data.train, data.test){
  data.tmp = data.train[, c(1, 3, 11:12, 14:25, 26:49, 51:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)

  X.mat = as.matrix(data.tmp[,2:ncol(data.tmp)])
  y.mat = as.vector(data.tmp$o3)
  
  case1 = which(is.na(rowSums(X.mat)))
  case2 = which(is.na(y.mat))
  case = unique(c(case1, case2))

  fit = glmnet(x=X.mat[-case,], y=y.mat[-case], lambda = seq(0,1,0.1), 
		alpha=0, family="gaussian")
  pred = predict(fit, as.matrix(data.test[, c(3, 11:12, 14:25, 26:49, 51:59)-1]))
  pred = pred[, round(1+ncol(pred)/2)]
  pred = pred^2
  return(pred)  
}
# -------------------------------------
# -------------------------------------

 # -------------------------------------
# -------------------------------------
library(glmnet)
# Model 15:
model_15 = function(data.train, data.test){
  data.tmp = data.train[, c(1, 3, 11:12, 14:25, 26:49, 51:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)

  X.mat = as.matrix(data.tmp[,2:ncol(data.tmp)])
  y.mat = as.vector(data.tmp$o3)
  
  case1 = which(is.na(rowSums(X.mat)))
  case2 = which(is.na(y.mat))
  case = unique(c(case1, case2))

  fit = glmnet(x=X.mat[-case,], y=y.mat[-case], 
		alpha=1, family="gaussian")
  pred = predict(fit, as.matrix(data.test[, c(3, 11:12, 14:25, 26:49, 51:59)-1]))
  pred = pred[, round(1+ncol(pred)/2)]
  pred = pred^2
  return(pred)  
}
# -------------------------------------
# -------------------------------------
 


 # -------------------------------------
# -------------------------------------
library(glmnet)
# Model 16:
model_16 = function(data.train, data.test){
  data.tmp = data.train[, c(1, 3, 6:10, 11:12, 14:25, 26:49)]
  data.tmp$o3 = sqrt(data.tmp$o3)

  X.mat = as.matrix(data.tmp[,2:ncol(data.tmp)])
  y.mat = as.vector(data.tmp$o3)
  
  case1 = which(is.na(rowSums(X.mat)))
  case2 = which(is.na(y.mat))
  case = unique(c(case1, case2))

  fit = glmnet(x=X.mat[-case,], y=y.mat[-case], 
		alpha=1, family="gaussian")
  pred = predict(fit, as.matrix(data.test[, c(3, 6:10, 11:12, 14:25, 26:49)-1]))
  pred = pred[, round(1+ncol(pred)/2)]
  pred = pred^2
  return(pred)  
}
# -------------------------------------
# -------------------------------------
 
# Model 20:
library(glmnet)
model_30 = function(data.train, data.test){
  data.tmp = data.train[, c(1, 3,26:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  
  X.mat = as.matrix(data.tmp[,2:ncol(data.tmp)])
  y.mat = as.vector(data.tmp$o3)
  
  case1 = which(is.na(rowSums(X.mat)))
  case2 = which(is.na(y.mat))
  case = unique(c(case1, case2))
  
  cv.fit = cv.glmnet(x=X.mat[-case,], y=y.mat[-case],type.measure = "mse", alpha=1, family="gaussian")
  plot(cv.fit)
  fit = glmnet(x=X.mat[-case,], y=y.mat[-case], lambda=cv.fit$lambda.1se ,alpha= 0, family="gaussian")
  pred = predict(fit, as.matrix(data.test[, c(2,25:58)]))
  #pred = pred[, round(1+ncol(pred)/2)]
  pred = pred^2
  return(pred)  
}

pred = model_30(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M30:lasso"
model.output[[1]] = output

modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")

# Model 21:
model_21 = function(data.train, data.test){
  data.tmp = data.train[, c(1,3,53:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  data.tmp.2 = data.test[, c(2,5:11,25:49,52:58)]
  
  X.mat.1 = as.matrix(data.tmp[,2:ncol(data.tmp)])
  X.mat.2 = as.matrix(data.tmp.2[,1:ncol(data.tmp.2)])
  X.mat = rbind(X.mat.1, X.mat.2)
  pr.out = prcomp(X.mat, scale=FALSE)
  y.mat = as.vector(data.tmp$o3)

  Z = data.frame( pr.out$x[,1:3]) # keep the first M PC
  data.train.new = Z[1:nrow(X.mat.1),]
  data.train.new$o3 = y.mat
  data.test.new = Z[(nrow(X.mat.1)+1):nrow(X.mat),]
  
  fit = lm(o3~., data.train.new)
  pred = predict(fit, data.test.new)
  pred = pred^2
  return(pred) 
}
pred = model_21(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M21: pcr"
model.output[[4]] = output

modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")


# Model 22:
library(glmnet)
model_22 = function(data.train, data.test){
  data.tmp = data.train[, c(1,3,11:12,14:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  
  X.mat = as.matrix(data.tmp[,2:ncol(data.tmp)])
  y.mat = as.vector(data.tmp$o3)
  
  case1 = which(is.na(rowSums(X.mat)))
  case2 = which(is.na(y.mat))
  case = unique(c(case1, case2))
  
  fit = glmnet(x=X.mat[-case,], y=y.mat[-case], lambda = 0.01, 
               alpha=0, family="gaussian")
  pred = predict(fit, as.matrix(data.test[, c(2,10:11, 13:58)]))
  #pred = pred[, 11]
  pred = pred^2
  return(pred)   
}
pred = model_22(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M22: lambda"
model.output[[5]] = output

modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")

# Model 40:
model_40 = function(data.train, data.test){
  data.tmp = data.train[, c(1,3,6:12,26:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  data.tmp.2 = data.test[, c(2,5:11,25:58)]
  
  pcr.fit = pcr(o3~., data=data.tmp,scale=FALSE,validation = "CV")
  summary(pcr.fit)
  validationplot(pcr.fit,val.type="MSEP" ) # val.type="MSEP"
  pred = predict(pcr.fit, data.tmp.2,ncomp=10)
  pred = pred^2
  mean((pred-obs)^2)
  return(pred) 
}
pred = model_40(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M21: pcr"
model.output[[1]] = output

modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")


# Model 50:
library(leaps)
model_50 = function(data.train, data.test){
  data.tmp = data.train[, c(1,3,6:12,26:59)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  data.tmp.2 = data.test[, c(2,5:11,25:58)]
  
  pls.fit = plsr(o3~., data=data.tmp, scale = F)#, validation = "CV")
  pred = predict(pls.fit, data.tmp.2,ncomp=33)
  pred = pred^2
  return(pred) 
}
pred = model_50(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M50: pls2"
model.output[[3]] = output

modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")


# Model60
library(glmnet)
model_60 = function(data.train, data.test){
  data.tmp = data.train[, c(1,3,6:12,26:49,51:52)]
  data.tmp$o3 = sqrt(data.tmp$o3)
  
  X.mat = as.matrix(data.tmp[,2:ncol(data.tmp)])
  y.mat = as.vector(data.tmp$o3)
  
  
  cv.fit = cv.glmnet(x=X.mat, y=y.mat,nfolds = 10, type.measure = "mse", alpha=1, family="gaussian")
  plot(cv.fit)
  fit = glmnet(x=X.mat, y=y.mat, lambda=cv.fit$lambda.min ,alpha= 1, family="gaussian")
  pred = predict(fit, as.matrix(data.test[, c(2,5:11,25:48,50:51)]))
  #pred = pred[, round(1+ncol(pred)/2)]
  pred = pred^2
  return(pred)  
}

pred = model_60(data.train, data.test)
output = data.frame(cbind(pred, obs))
colnames(output) = c("pred","obs")
output$model = "M30:lasso"
model.output[[1]] = output

modData = do.call(rbind,model.output)
modStats(modData, obs = "obs", mod = "pred", type = "model")
