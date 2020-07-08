data <- read.csv(file="Advertising.csv", header = T, stringsAsFactors = F, row.names = 1)
head(data)
plot(data, col = "blue")


library("ggplot2")
library("ggExtra")
??"geom_"
qplot(data$sales,data$TV)+geom_point(shape=6)


p <- ggplot(data, aes(x=TV,y=sales))+geom_point()+theme(legend.position = "none")
p
ggMarginal(p, type="histogram")
attach(data)
pairs(data)
summary(data)

library("MASS")
head(survey)
plot(survey$Height~survey$Wr.Hnd)

model_1 <- lm(sales~radio,data=data)
model_1
summary(model_1)
length(data$sales)
ggplot(data,aes(radio,sales))+geom_point()+geom_smooth(method = "lm")
abline(model_1)      
plot(data$sales~data$radio)
names(model_1)
confint(model_1,level = 0.95)
summary(survey$Smoke)
plot(model_1)

# Lecture 3 code

set.seed(10)
x <- runif(100,0,1)
error = rnorm(100,0,5)
y = 2+2*x+error
data_2 = data.frame(y = y, x = x)
head(data_2)
ols_fit_2 = lm(y~x,data)
summary(ols_fit_2)


# Assignment Lecture 4

income_data = read.csv(file = "Income2.csv", header = T, stringsAsFactors = F, row.names = 1)
head(income_data)
attach(income_data)
model_income = lm(Income~Education+Seniority)
summary(model_income)

plot(model_income)

# LINEAR REGRESSION CODE

library("MASS")
head(Boston)
dim(Boston)
fix(Boston)
names(Boston)

# CONFIDENCE INTERVAL

lm.fit = lm(medv~lstat, Boston)
summary(lm.fit)
confint(lm.fit)
fix(Boston)
predict(lm.fit,data.frame(lstat=c(5,10,15)), interval = "confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)), interval = "prediction")
attach(Boston)
plot(lstat,medv)
abline(lm.fit)
plot(1:20,1:20,pch =1:20) # visualize the point characters
par(mfrow = c(2,2)) # Splits the screen into four section
plot(lm.fit)
residuals(lm.fit)
rstudent(lm.fit)
predict(lm.fit)
hatvalues(lm.fit)
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# MULTIPLE LINEAR REGRESSION

lm.fit_2 = lm(medv~.,Boston)
summary(lm.fit_2)
names(lm.fit_2)
lm.fit_2$coefficients
summary(lm.fit_2)$r.sq
library("car")
vif(lm.fit_2)

lm.fit_3 = lm(medv~lstat+I(lstat^2))
plot(lm.fit_3)

# MODEL SELECTION Cp Mallows

data <- read.csv(file="Advertising.csv", header = T, stringsAsFactors = F, row.names = 1)
head(data)
ols.fit = lm(sales~.,data = data)

n = nrow(data)
p = 3

ols.fit_1 = lm(sales~.,data = data)
d = 3
summary(ols.fit_1)
names(summary(ols.fit_1))
sigma = summary(ols.fit_1)$sigma 
df_resid = summary(ols.fit_1)$df[2]
RSS = sigma^2 * df_resid
Cp_1 = 1/n*(RSS+2*d*sigma^2)
Cp_1

#BEST SUBSET SELECTION


set.seed(1)
x1 =  rnorm(100,0,1) #simulate x1
x2 = rnorm(100,0,1) #simulate x2
x3 = rnorm(100,0,1) #simulate x3
x4 = rnorm(100,0,1) #simulate x4
e = rnorm(100,0,0.4) #simulate noise
y = 1*x1 + 2*x2 + e #simulate y; note that, x3 and x4 are irrelevant, and we hope the best subset selection can identify the important variables

data.x = data.frame( cbind(x1, x2, x3, x4) )


p =  4  # number of candidate covariates

for (d in 1:p){ 
  combination = t(combn(p, d))
  m = nrow( combination )
  print(m)
  for (j in 1:m){
    data.cut = data.x[, combination[j,]]
    data.0 = data.frame(  cbind(y, data.cut) )
    fit =  lm(y~., data.0)
    adj.R2 = summary(fit)$adj.r.squared
    print(c(adj.R2, combination[j,]))
  }
}

#BEST SUBSET SELECTION

install.packages("lasso2")
library("lasso2")
data("Prostate")
head(Prostate)

data = Prostate
p = 8
n = nrow(Prostate)
model.list = list()
adj.R2 = array()
class(adj.R2)

jj = 1
for (d in 1:p){
  combination = combn(p,d)
  n.com = ncol(combination)
  for (j in 1:n.com){
    data.cut = data[, c(combination[,j], 9)]
    fit = lm(lpsa~., data.cut)
    model.list[[jj]] = fit
    adj.R2[jj] = summary(fit)$adj.r.squared
    jj = jj + 1
    
  }
}
length(adj.R2)

plot(adj.R2, type="h", col="blue", lwd=2)

opt = which(adj.R2 == max(adj.R2))
model.list[[opt]]
array(1:10,2)


# VARIABLE SELECTION USING AIC

# -----------------
library("MASS")

fit.all = lm(lpsa~.,Prostate)
fit.null = lm(lpsa~1, Prostate)

stepAIC(fit.null, direction="forward", 
        scope=list(lower=fit.null,upper=fit.all))

stepAIC(fit.all, direction="backward")

stepAIC(fit.all, direction=c("both"),
        scope=list(lower=fit.null,upper=fit.all))
?stepAIC



# VARIABLE SELECTION USING REGSUBSETS

install.packages("leaps")
library("leaps")

fit <- regsubsets(lpsa~., nbest=1, data=data, method="backward")
summary(fit)
plot(fit, scale="bic")
plot(fit, scale="adjr2")
plot(fit, scale="Cp")
summary(fit)$bic

fit <- regsubsets(lpsa~1, nbest=1,data=data, method="forward")
plot(fit, scale="bic")
plot(fit, scale="adjr2")
plot(fit, scale="Cp")

fit <- regsubsets(lpsa~., nbest=1,data=data, method="seqrep")
plot(fit, scale="bic")
plot(fit, scale="adjr2")
plot(fit, scale="Cp")

# RELATIVE IMPORTANCE 

install.packages("relaimpo")
library('relaimpo')
?calc.relimp
data <- read.csv(file="Credit.csv", header = T, stringsAsFactors = F, row.names = 1)
head(data)
fit.ols = lm(Balance~Income+Limit+Rating+Cards+Age+Education, data = Credit)
summary(fit.ols)
calc.relimp.lm(fit.ols,type = "lmg",rela = T)

# BOOTSTRAP MEASURE OF RELATIVE IMPORTANCE

?boot.relimp
?booteval.relimp
boot = boot.relimp(fit.ols, rela = T)
plot(booteval.relimp(boot,sort =T))

# VIF CALCULATION BY HAND

data <- read.csv(file="Credit.csv", header = T, stringsAsFactors = F, row.names = 1)
head(data)
names(summary(r))
Rsq = summary(r)$r.squared
VIF =1/(1-Rsq) 
VIF



# VIF CALCULATION professor

install.packages("fmsb")
library(fmsb)
data("Prostate")
head(Prostate)
data = Prostate
data.X = data[, -9]
dim(data)
dim(data.X)
vif = array(0, dim=c(8,1))

vif[1] = VIF(lm(lcavol  ~ ., data=data.X))
vif[2] =VIF(lm(lweight   ~ ., data=data.X))
vif[3] =VIF(lm(age ~ ., data=data.X))
vif[4] =VIF(lm(lbph ~ ., data=data.X))
vif[5] =VIF(lm(svi  ~ ., data=data.X))
vif[6] =VIF(lm(lcp ~ ., data=data.X))
vif[7] =VIF(lm(gleason ~ ., data=data.X))
vif[8] =VIF(lm(pgg45~ ., data=data.X))
vif = as.numeric(vif)
vif
?barplot()
barplot(as.numeric(vif), names=colnames(data.X), col="blue")

data.X2 = data[, 1:5]
vif = array(0, dim=c(5,1))
vif[1] = VIF(lm(lcavol  ~ ., data=data.X2))
vif[2] =VIF(lm(lweight   ~ ., data=data.X2))
vif[3] =VIF(lm(age ~ ., data=data.X2))
vif[4] =VIF(lm(lbph ~ ., data=data.X2))
vif[5] =VIF(lm(svi  ~ ., data=data.X2))
barplot(as.numeric(vif), names=colnames(data.X2), col="blue")

# RIDGE RIGRESSION USING RIDGE PACKAGE

library("lasso2")
data = Prostate
head(data)

# data scaling
for (i in 1:8){
  data[,i] = (data[,i] - mean(data[,i]))/sqrt(var(data[,i]))
}

install.packages("ridge")
library("ridge")
?linearRidge
fit.ridge = linearRidge(lpsa~., data, lambda = seq(0,20,0.1), scaling="none")
plot(fit.ridge)


# RIDGE RIGRESSION USING MASS PACKAGE


library(MASS)
?lm.ridge()
fit.mass = lm.ridge(lpsa ~ ., data,lambda = seq(0,20,0.1))
plot(fit.mass,label=TRUE)

# RIDGE RIGRESSION USING glmnet PACKAGE

install.packages("glmnet")
library(glmnet)
X = as.matrix(data[,1:8])
y = as.vector(data$lpsa)
?glmnet()
fit.glmnet = glmnet(x=X, y, lambda = seq(0,20,0.1), standardize = FALSE, alpha=0)
plot(fit.glmnet, xvar="lambda")


# HOMEWORK RIDGE REGRESSION ON CREDIT DATA


data2 <- read.csv(file="Credit.csv", header = T, stringsAsFactors = F, row.names = 1)
head(data2)

summary(data2)
table(data2$Gender)

# convert "Gender" to 0 and 1
case = which(data2$Gender=="Male")
data2$Gender[case] = 1
data2$Gender[-case] = 0

?apply()
apply(data2, 2, class)
for (i in 1:ncol(data2)){
  data2[,i]=as.numeric(data2[,i])
}



# convert "Student" to 0 and 1
case = which(data2$Student=="Yes")
data2$Student[case] = 1
data2$Student[-case] = 0

# convert "Married" to 0 and 1
case = which(data2$Married=="Yes")
data2$Married[case] = 1
data2$Married[-case] = 0

# Create two dummy columns, "Caucasian", "Asian"
# If a person is Caucasian, the Caucasian column is set to 1, and the Asian column is set to 0
# If a person is Asian, the Caucasian column is set to 0, and the Asian column is set to 1
# If a person is African American, the Caucasian column is set to 0, and the Asian column is set to 0

data2$Caucasian = 0
case = which(data2$Ethnicity=="Caucasian")
data2$Caucasian[case] = 1

data2$Asian = 0
case = which(data2$Ethnicity=="Asian")
data2$Asian[case] = 1

# Remove the original "Ethnicity" column
data2 = data2[,-which(colnames(data2)=="Ethnicity")]

for (i in 1:8){
  data2[,i] = (data2[,i] - mean(data2[,i]))/sqrt(var(data2[,i]))
}

library(MASS)
?lm.ridge()
fit.mass = lm.ridge(lpsa ~ ., data2,lambda = seq(0,20,0.1))
plot(fit.mass,label=TRUE)


# LASSO REGRESSION USING (glmnet)


library(lasso2)
data(Prostate)
data = Prostate
head(data)

for (i in 1:8){
  data[,i] = (data[,i] - mean(data[,i]))/sqrt(var(data[,i]))
}
library(glmnet)
X.mat = as.matrix(data[,1:8])
y.mat = as.vector(data$lpsa)

fit.glmnet = glmnet(x=X.mat, y=y.mat, lambda = seq(0,4,0.01),standardize = F, alpha=1)
plot(fit.glmnet, label = TRUE, xvar="lambda")
#alpha=1 is the lasso penalty, and alpha=0 the ridge penalty

cv.out = cv.glmnet(x=X.mat, y=y.mat, nfolds=4)
plot(cv.out)
cv.out$lambda.min  # lambda selection using cross-validation

?cv.glmnet

# PRINCIPLE COMPONENT ANALYSIS (PCA) PART 1

data("USArrests")
data  = USArrests
head(data)
states = row.names(data)
variable = names(data)

for(i in 1:ncol(data)){
  data[,i] = (data[,i]-mean(data[,i]))/sqrt(var(data[,i]))
}
plot(data)

pr.out = prcomp(data, scale=FALSE)
print(pr.out)
names(pr.out)
pr.out$x
?biplot()
biplot(pr.out,scale=0)
plot(data)

# CROSS VALIDATION

library("ISLR")
dim(Auto)
set.seed(1)
train  = sample(392,196)
lm.fit = lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg -predict (lm.fit ,Auto))[-train ]^2) # MSE for validation set 


#-------------LOGISTIC REGRESSION-------------------------
#----------response as.factor or numeric does not matter---------
library("ISLR")
data("Smarket")
head(Smarket)
dim(Smarket)
cor(Smarket[,-9])
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family = binomial)
summary(glm.fit)
contrasts(Smarket$Direction) # tells us which category is "1" and which is "0".
glm.prob = predict(glm.fit, type = "response") # produces probabilities P(Y = 1|X). Default: for training data
length(glm.prob)
glm.pred=rep("Up",1250)
glm.pred[glm.prob<0.5] = "Down"


table(glm.pred,Smarket$Direction) # creates "CONFUSION MATRIX", how many obs were correctly or incorrectly classified
                                  # diaganol: correct prediction, off-diagonal: incorrect prediction      
correct.pred = (145+507)/1250  # correct predictions
mean(glm.pred==Smarket$Direction) # fraction of response where prediction was correct
train.err.rate = 1-correct.pred

# let's try training data

train = Smarket$Year<2005
Smarket.test = Smarket[!train,]
dim(Smarket.test)
Direction.test = Smarket.test$Direction
glm.fit.train = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket[train,], family = binomial)
glm.probs = predict(glm.fit.train, Smarket.test, type = "response")
glm.pred = rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Direction.test)
length(Direction.test)
mean(glm.pred==Direction.test)
mean(glm.pred!=Direction.test) # <- TEST ERROR RATE
summary(glm.fit.train)
--------------------#####################################----------------------------------------