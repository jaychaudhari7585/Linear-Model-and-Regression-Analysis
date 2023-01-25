#Q.1
Age=c(22,19,23,31,21,41,27,26,18,34,31,28,24,20,21,19,35)
Height=c(175,152,165,162,193,137,182,162,172,169,172,160,163,185,190,168,137)
DBP=c(60,70,82,90,68,76,76,62,74,76,70,80,62,78,75,68,60)
SBP=c(122,102,118,108,120,104,120,116,118,102,112,122,118,124,120,102,106)
Volume=c(3.1,3.4,3,3.2,4.9,2.4,4.5,3.1,4.4,2.9,4.2,3,3,4.7,4.8,4.1,2.3)
Data=data.frame(Volume,Age,Height,DBP,SBP)
#a) Multiple scatter plot 
plot(Data)
#b) Fitting MLRM 
model=lm(Volume~Age+Height+DBP+SBP)
# i)Test of sign. of regression and conclusion,
summary(model)
anova(model)
pred_y=predict(model)
model$residuals

par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
hist(model$residuals)
# variance covariance matrix of the estimated parameters
X=cbind(rep(1,length(Age)),Age,Height,DBP,SBP)
library(MASS)
library(Matrix)
r=rankMatrix(X)
S=t(X)%*%X
G=solve(S)
H=G%*%S
beta_hat=G%*%t(X)%*%Volume
SSE=t(Volume)%*%Volume-t(beta_hat)%*%t(X)%*%Volume
sigma=SSE/(length(Age)-r[1])
var_covar_of_beta_hat=sigma^2%*%G
par(mfrow=c(1,1))
plot(pred_y,model$residuals)

# confidence interval
confint(model,level=0.95) ##confint(model,interval="confidence")
confint(model,interval='prediction')
predict(model,interval="confidence")
predict(model,interval = "prediction")
predict(model,data.frame(Age=21,Height=165,DBP=80,SBP=115),interval='prediction')
predict(model,data.frame(Age=c(21,23),Height=c(165,170),DBP=c(80,82),SBP=c(115,118)),interval='prediction')

## split regression sum of squares 
## To test is DBP and SBP contributes to the model
model_1=lm(Volume~Age+DBP+SBP)
anova(model_1,model)
SS_H=2.8004-2.5900
F0=SS_H/0.2158


## Que.2
y=c(2256,2340,2426,2293,2330,2368,2250,2409,2364,2379,2440,2364,2404,2317,2309,2328)
x1=c(80,93,100,82,90,99,81,96,94,93,97,95,100,85,86,87)
x2=c(8,9,10,12,11,8,8,10,12,11,13,11,8,12,9,12)
data=data.frame(y,x1,x2)
plot(data)
fit=lm(y~x1+x2)
fit
anova(fit)
summary(fit)
##variance covariance matrix of beta_hat
x=cbind(rep(1,16),x1,x2)
library(MASS)
library(Matrix)
r=rankMatrix(x)
s=t(x)%*%x
G=ginv(s)
H= G%*%s
beta_hat=G%*%t(x)%*%y
sse=t(y)%*%y-t(beta_hat)%*%t(x)%*%y
sigma=sse/(n-r[1])
var_covar_of_beta_hat=sigma%*%H%*%G



##Que3
y=c(650,1200,1300,430,1400,900,1800,640,793,925)
x=c(2,7,9,4,12,6,9,3,3,2)
plot(x,y)
##fitting 
fit=lm(y~x)
summary(fit)
anova(fit)
confint(fit,interval='confidence')
predict.lm(fit,data.frame(x=5),interval = 'prediction')
predict.lm(fit,interval = 'prediction')
predict.lm(fit,interval = 'confidence')
confint(fit,interval='prediction')
