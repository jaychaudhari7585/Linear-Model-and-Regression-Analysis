n1=17
n2=9
n3=12
n=n1+n2+n3
x1=c(rep(1,n))
x2=c(rep(1,n1),rep(0,n2+n3))
x3=c(rep(0,n1),rep(1,n2),rep(0,n3))
x4=c(rep(0,n1+n2),rep(1,n3))
x=cbind(x1,x2,x3,x4)
s=t(x)%*%x
library(Matrix)
r=rankMatrix(s)
r=qr(s)$rank
library(MASS)
G=ginv(s)
y1=c(3.1,3.5,4.8,3.1,5.5,5.0,4.8,4.3,4.7,4.3,5.1,3.0,4.3,5.5,4.2,3.5,5.7)
y2=c(4.3,3.4,5.0,4.1,4.7,5.0,5.0,2.9,5.0)
y3=c(3.9,4.5,7.0,6.7,5.8,5.6,4.8,5.5,6.6,5.3,5.7,6.0)
y=matrix(c(y1,y2,y3),n,1)
beta_hat=G%*%t(x)%*%y
y_hat=x%*%beta_hat
e=y-y_hat
e
SEE=t(e)%*%e
SStret=t(beta_hat)%*%t(x)%*%y-n*mean(y)^2
TSS=SEE+SStret
SST=t(y)%*%y-n*mean(y)^2
f=(SStret*(n-r))/(SEE*(r-1))
f
fcal=qf(0.05,r-1,n-r,lower.tail = FALSE)


### shorcut method
boxplot.default(y1,y2,y3)
factor=c(rep("y1",n1),rep("y2",n2),rep("y3",n3))
data=data.frame(y,factor)
fit=aov(y~factor)
fit
summary(fit)
TukeyHSD(fit,conf.level=0.95)

# Q.2
A=c(1020,1010,1030,1000);n1=length(A)
B=c(1030,1040,1050,1030,1060);n2=length(B)
C=c(990,980,970,960,970,980);n3=length(C)
D=c(1040,1050,1030,1070);n4=length(D)
n=n1+n2+n3+n4
y=matrix(c(A,B,C,D),n,1)
factor=c(rep('A',n1),rep('B',n2),rep('C',n3),rep('D',n4))
data=data.frame(y,factor)
fit1=aov(y~factor)
summary(fit1)
TukeyHSD(fit1,conf.level = 0.95)
## Q.3
y1=c(3.22,3.04,3.06,2.64,3.19,2.49)
y2=c(3.31,2.99,3.17,2.75,3.40,2.37)
y3=c(3.26,3.27,2.93,2.59,3.11,2.38)
y4=c(3.25,3.20,3.09,2.62,3.23,2.37)

boxplot(y1,y2,y3,y4)
n=length(y1)+length(y2)+length(y3)+length(y4)
y=matrix(c(y1,y2,y3,y4),n,1)
field=c(rep('1',length(y1)),rep('2',length(y2)),rep('3',length(y3)),rep('4',length(y4)))
variety=c(rep('A',4),rep('B',4),rep('c',4),rep('D',4),rep('E',4),rep('F',4))
Fit3=aov(y~field+variety)
summary(Fit3)
TukeyHSD(Fit3)
