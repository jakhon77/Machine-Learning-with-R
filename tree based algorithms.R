

#----------REGRESSION TREE-----------Much better way
library(rpart)

library(ISLR)
data("Hitters")
head(Hitters)
dim(Hitters)
Hitters$Salary = log(Hitters$Salary)

# construct a decision tree

cart.tree = rpart(Salary~.,data=Hitters,method="anova", c=0.005)# putting c is optional because the right value will be identified through plotcp
plot(cart.tree, margin=0.04, uniform = TRUE)
text(cart.tree)

# cross-validation and pruning

plotcp(cart.tree) #plot the model performance against cp, which controls the tree complexity
cart.tree.2 = prune(cart.tree, cp=0.042 ) #prune the previous tree using the cp chosen from the plot above
plot(cart.tree.2, margin=0.04)
text(cart.tree.2)

##--------BOOK REGRESSION TREE----------Too complicated

install.packages("tree")
library(tree)

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston = tree(medv~.,Boston, subset = train)

summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type="b")

prune.boston=prune.tree(tree.boston ,best=5)
plot(prune.boston)
text(prune.boston , pretty =0)

yhat=predict (tree.boston ,newdata=Boston[-train ,])
boston.test=Boston[-train ,"medv"]
plot(yhat ,boston.test)
abline (0,1)
mean((yhat -boston.test)^2)

##----------------------###############

# -------------------
# Construct a decision tree for Project 1 data 
# Load Data (Please Modify the Following two lines to load data)
data.train = read.csv("C:/Users/jkhat/Downloads/Project_1_stat/data_train.csv", header = TRUE, stringsAsFactors = FALSE)
data.test = read.csv("C:/Users/jkhat/Downloads/Project_1_stat/data_test.csv", header = TRUE, stringsAsFactors = FALSE)

data.cut = data.train[, c(1, 3, 11:12, 26:49, 51:52)] #select columns (variables) to be included in the model

# a collection of control parameter settings for the decisison tree 
# for the meanings of all control parameters: https://www.rdocumentation.org/packages/rpart/versions/4.1-13/topics/rpart.control f
control = rpart.control(minsplit =20, cp = 0.001, 
                        maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
                        surrogatestyle = 0, maxdepth = 30) 
# grow the tree
cart.p1 = rpart(o3~., 
                data=data.cut,  
                method="anova",
                control=control) #method="class" for a classification tree; method="anova" for a regression tree
plot(cart.p1, margin=0.04, uniform=TRUE)
text(cart.p1)

plotcp(cart.p1) #plot the model performance against cp, which controls the tree complexity
cart.p1.2 = prune(cart.p1, cp=0.0026 ) #prune the previous tree using the cp chosen from the plot above
plot(cart.p1.2, margin=0.04, uniform=TRUE)
text(cart.p1.2)
# ------------------- 


# ----------------- CLASSIFICATION TREE------------------------- 
# Data:
library(ISLR)
data("Hitters")
Hitters$Log_Salary = log(Hitters$Salary)
head(Hitters)

# Classification Tree
# Create two categories based on salary; 
Hitters$Cat_Salary = 1
case = which(Hitters$Salary > 536)
Hitters$Cat_Salary[case] = 2
head(Hitters[,c("Cat_Salary","Years","Hits")],20)

cart.tree = rpart(Cat_Salary~Years+Hits, 
                  data=Hitters,  
                  method="class") 
plot(cart.tree, margin=0.04)
text(cart.tree)

cart.tree = rpart(Cat_Salary~Years+Hits+Runs+RBI+Walks+PutOuts+Assists+Errors, 
                  data=Hitters,  
                  method="class") 
plot(cart.tree, margin=0.04)
text(cart.tree)

plotcp(cart.tree)
cart.tree.2 = prune(cart.tree, cp=0.056)
plot(cart.tree, margin=0.04)
text(cart.tree)

# Heart data, classification tree
data = read.csv("Heart.csv", 
                stringsAsFactors = TRUE, header=TRUE, row.names=1)
head(data)
data$Sex = as.factor(data$Sex) # define "sex" column as factors

cart.tree = rpart(AHD~., 
                  data=data,  
                  method="class", cp=0.001) 
plot(cart.tree, margin=0.04, uniform=TRUE)
text(cart.tree)

unique(data$Thal) # this returns the levels for Thal
unique(data$ChestPain)
unique(data$Ca)

plotcp(cart.tree)
cart.tree.2 = prune(cart.tree, cp=0.018)
plot(cart.tree.2, margin=0.04)
text(cart.tree.2)


#---------RANDOM FOREST----------------

install.packages("ElemStatLearn")
library(ElemStatLearn) # load this package so that we could use the spam data
data(spam)
head(spam)

# ----------------------------
# Classification tree on spam data
# ----------------------------
library(rpart)

cart.tree = rpart(spam~., 
                  data=spam,  
                  method="class") #method="class" for a classification tree; method="anova" for a regression tree
plot(cart.tree, margin=0.04)
text(cart.tree)
plotcp(cart.tree)
cart.tree.2 = prune(cart.tree, cp=0.018)
plot(cart.tree.2, margin=0.04)
text(cart.tree.2)
# ----------------------------
# RF on spam data
# ----------------------------
# training
library(randomForest) # this is the Random Forests package we need
fit = randomForest(spam~.,spam,ntree=1000,do.trace=100)
# build a RF with 1000 trees; report the results every 100 trees.

# training
fit = randomForest(spam~.,spam,ntree=500,do.trace=100,
                   importance=TRUE,confusion=TRUE)
fit$importance # generate the variable importance ranking table
varImpPlot(fit) # generate the variable importance ranking plot
fit$confusion # generate the confusion matrix

# ------------------
# Homework: RF on project 1 data (illustration)
# ------------------
# Load Data
setwd("C:/Users/xl027/Desktop/UARK/courses/2019Spring/code")
data.train = read.csv("/Users/jkhat/Downloads/Project_1_stat/data_train.csv", header = TRUE, stringsAsFactors = FALSE)
data.test = read.csv("C:/Users/jkhat/Downloads/Project_1_stat/data_test.csv", header = TRUE, stringsAsFactors = FALSE)

data.tmp = data.train[, c(1,3,6:12,14:59)] # select some variables to be included in the model
case = sample(1:nrow(data.tmp),5000 ) # select 5000 rows for build the model; You don't have to do this, but building the model using all data will be much slower. 
fit = randomForest(o3~., data.tmp[case,], na.action=na.omit,importance=TRUE,
                   ntree=500,do.trace=100)
varImpPlot(fit) # see the variable importance ranking



#-------------RANDOM FOREST CONTINUE-------------- The following code is used for Lecture 15. 

data = read.csv("housing.csv", stringsAsFactors=FALSE, header=TRUE)
data[,10] = as.factor(data[,10])
summary(data)

# memory.size(max=FALSE) 
# memory.limit(size=9000000)

# random forest
library(randomForest)
fit = randomForest(median_house_value~.,
                   data,ntree=500,do.trace=50,na.action=na.omit,importance=TRUE,
                   proximity=TRUE)
save(fit, file="L19_fit.RData")
load("L19_fit.RData")
library(randomForest)
# mse over # trees
par(mfrow=c(1:2))
plot(log(c(1:500)), fit$mse,col="darkgreen",xlab="# trees grown", ylab="mse") # I don't know why we need log. Better without log
plot((c(1:500)), fit$mse,col="darkgreen",xlab="# trees grown", ylab="mse")

# variable importance
varImpPlot(fit)

# proximity:
case = sample(1:nrow(data),1000)
fit.2 = randomForest(median_house_value~.,
                     data[case,],ntree=500,do.trace=50,na.action=na.omit,importance=TRUE,
                     proximity=TRUE)
proxi = fit.2$proximity
plot(proxi[200,],type="h",ylab="proximity",xlab="data points",col="blue")

# PCA
for (i in 1:ncol(proxi)){
  proxi[,i]=(proxi[,i]-mean(proxi[,i]))/sqrt(var(proxi[,i]))
}
pr.out = prcomp(proxi, scale=FALSE)


library(rgl) # package for 3D visualization
plotPCA <- function(x, nGroup, text) {  # user-defined function
  n <- ncol(x) 
  if(!(n %in% c(2,3))) { # check if 2d or 3d
    stop("x must have either 2 or 3 columns")
  }
  
  fit <- hclust(dist(x), method="complete") # cluster
  groups <- cutree(fit, k=nGroup)
  
  if(n == 3) { # 3d plot
    plot3d(x, col="green", type="s", size=1, axes=F)
    axes3d(edges=c("x--", "y--", "z"), lwd=3, axes.len=2, labels=FALSE)
    text3d(x,texts=text)
    grid3d("x")
    grid3d("y")
    grid3d("z")
  } else { # 2d plot
    maxes <- apply(abs(x), 2, max)
    rangeX <- c(-maxes[1], maxes[1])
    rangeY <- c(-maxes[2], maxes[2])
    plot(x, col=groups, pch=19, xlab=colnames(x)[1], ylab=colnames(x)[2], xlim=rangeX, ylim=rangeY)
    lines(c(0,0), rangeX*2)
    lines(rangeY*2, c(0,0))
  }
}
plotPCA(pr.out$x[,1:3],2,text=rownames(data[case,]))



# Now, I'll separate the data into two parts; 
# The first part will be used for traning; 
# The second part will be used for testing; 
train.case = sample(1:nrow(spam),3500) # randomly selected 3500 rows for training
train.data = spam[train.case,] # create the training set
test.data = spam[-train.case,] # create the test set
fit = randomForest(spam~.,train.data,ntree=500,do.trace=100,confusion=TRUE)
pred = predict(fit, test.data) # generate the prediction results



#----------HOMEWORK-------------------

library(randomForest) # this is the Random Forests package we need
library(ElemStatLearn) # load this package so that we could use the spam data
data(spam)

fit = randomForest(spam~.,spam,ntree=1000,do.trace=100,proximity=TRUE)

# plot of overall classification error
plot((c(1:1000)), fit$err.rate[,1],col="darkgreen",xlab="# trees grown", ylab="error rate")
# plot of the error rate when emails are classified as spam
plot((c(1:1000)), fit$err.rate[,2],col="darkgreen",xlab="# trees grown", ylab="error rate")
# plot of the error rate when spam are classified as email
plot((c(1:1000)), fit$err.rate[,3],col="darkgreen",xlab="# trees grown", ylab="error rate")

# variable importance
varImpPlot(fit)

# proximity and PCA for clustering
proxi = fit$proximity
plot(proxi[1,],type="h",ylab="proximity",xlab="data points",col="blue")


# PCA
for (i in 1:ncol(proxi)){
  proxi[,i]=(proxi[,i]-mean(proxi[,i]))/sqrt(var(proxi[,i]))
}
pr.out = prcomp(proxi, scale=FALSE)

# package for 3D visualization

library(rgl) 
plotPCA <- function(x, nGroup, text) {  # user-defined function
  n <- ncol(x) 
  if(!(n %in% c(2,3))) { # check if 2d or 3d
    stop("x must have either 2 or 3 columns")
  }
  
  fit <- hclust(dist(x), method="complete") # cluster
  groups <- cutree(fit, k=nGroup)
  
  if(n == 3) { # 3d plot
    plot3d(x, col="green", type="s", size=1, axes=F)
    axes3d(edges=c("x--", "y--", "z"), lwd=3, axes.len=2, labels=FALSE)
    text3d(x,texts=text)
    grid3d("x")
    grid3d("y")
    grid3d("z")
  } else { # 2d plot
    maxes <- apply(abs(x), 2, max)
    rangeX <- c(-maxes[1], maxes[1])
    rangeY <- c(-maxes[2], maxes[2])
    plot(x, col=groups, pch=19, xlab=colnames(x)[1], ylab=colnames(x)[2], xlim=rangeX, ylim=rangeY)
    lines(c(0,0), rangeX*2)
    lines(rangeY*2, c(0,0))
  }
}
plotPCA(pr.out$x[,1:3],2,text=rownames(data[case,]))

#----------------------###################################-----------------------------------------------

#-------------PROJECT-2--------------------------------------

rm(list=ls())
setwd("C:/Users/jkhat/Downloads/Project_2_stat")
data.test = read.csv("data.test.csv", stringsAsFactors=FALSE, header=TRUE)
data.test[,10] = as.factor(data.test[,10])

data.train = read.csv("data.train.csv", stringsAsFactors=FALSE, header=TRUE)
data.train[,10] = as.factor(data.train[,10])

#--------------REGULAR RANDOM FOREST----------------------

library(randomForest)
time.s = proc.time() 
fit = randomForest(median_house_value~.,
                   data.train,ntree=500,do.trace=50,na.action=na.omit,importance=TRUE,
                   proximity=TRUE)
time.e = proc.time()-time.s 
print(time.e)

fit$importance 
varImpPlot(fit)

plot((c(1:500)), fit$mse,col="darkgreen",xlab="# trees grown", ylab="mse")

pred1= predict(fit, data.test)
obs = data.test$median_house_value
plot(pred1,obs)
RMSE.rf = sqrt( mean( (pred1-obs)^2 ) )
print(RMSE.rf)
#------------------RANDOM FOREST USING SPARK----------------------------

library("SparkR")
sparkR.session(master = "local[4]", enableHiveSupport = FALSE,
               sparkConfig = list(spark.driver.memory = "1g"))

df.train = as.DataFrame(data.train) 
df.test = as.DataFrame(data.test)

time.s = proc.time()
fit.spark = spark.randomForest(df.train, median_house_value~.,
                               type = c("regression"), 
                               maxDepth = 14, 
                               maxBins = 100,
                               numTrees = 80, 
                               subsamplingRate = 0.9,
                               minInstancesPerNode = 2,
                               impurity = NULL, 
                               featureSubsetStrategy = "auto",
                               seed = NULL, 
                               minInfoGain = 0, checkpointInterval = 10, maxMemoryInMB = 256,
                               cacheNodeIds = FALSE)
time.e = proc.time()-time.s
print(time.e) 

pred = predict(fit.spark, df.test) 
pred = collect(pred) 
pred2 = pred$prediction
obs = data.test$median_house_value
plot(pred2,obs)
RMSE.rf.spark = sqrt( mean( (pred2-obs)^2 ) )
print(RMSE.rf.spark)


#------------RANDOM SURVIVAL FOREST and KAPLAN-MEIER-------------------------


# load library for K-M method
library(survival)
install.packages("KMsurv")
library(KMsurv)

# input data:
time = c(10,7,32,23,22,6,16,34,32,25,11,20,19,6,17,35,6,13,9,6,10)
status = c(1,1,0,1,1,1,1,0,0,0,0,0,0,1,0,0,1,1,0,0,0)
Leukemia = data.frame( cbind(time,status) )

# prepare data for analysis (create a "Surv" class in R):
data.obj <- Surv(Leukemia$time,Leukemia$status)
# get the KM estimate:
KM <- survfit(data.obj~1)
# plot the results:
plot(KM,xlab="time-to-relapse",ylab="survival probability")


# -----------------------------
# RSF: veteran data
# -----------------------------
install.packages('randomForestSRC')
library(randomForestSRC)
data(veteran) # load the veteran data
v.obj <- rfsrc(Surv(time, status)~., 
               data = veteran, ntree = 1000, tree.err=TRUE,
               importance=TRUE, proximity = TRUE)
# generate basic plots for RSF
plot(v.obj)

# plot survival curves for first "5" individuals: direct way
matplot(v.obj$time.interest, 100 * t(v.obj$survival.oob[1:5, ]),
        xlab = "Time", ylab = "Survival", type = "l", lty = 1)

# Suppose we randomly sample 100 individuals for training
# then, we predict the survival function for the remaining 37 individuals
set.seed(100)
train.set = sample(c(1:137),100) # randomly sample 37 individuals
data.train = veteran[train.set, ]
data.test = veteran[-train.set, ]
v.obj.2 <- rfsrc(Surv(time, status) ~ ., 
                 data = data.train, ntree =1000, tree.err=TRUE,
                 importance=TRUE, proximity = TRUE)
pred = predict(v.obj.2, data.test)

# plot the predicted survival function
matplot(pred$time.interest, 100 * t(pred$survival[1:37, ]),
        xlab = "Time", ylab = "Survival", type = "l", lty = 1)



 
