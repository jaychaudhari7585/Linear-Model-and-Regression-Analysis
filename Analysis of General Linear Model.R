#Que.1 (a)
y=c("y1","y2","y3")
x1=c(1,1,0)
x2=c(1,0,1)
x3=c(1,1,0)
#Design matrix of order n*p
x=rbind(x1,x2,x3);x
library(MASS)
library(Matrix)
r=rankMatrix(x);r[1]
s=t(x)%*%x
zapsmall(s)
G=ginv(s)
beta_hat=G%*%t(x)
#(b)

#Que.2
x1=c(1,0,0,0,1,0,0,0)
x2=c(0,1,0,0,1,0,0,0)
x3=c(0,0,1,0,1,0,0,0)
x4=c(0,0,0,1,0,1,0,0)
x5=c(1,0,0,0,0,0,1,0)
x6=c(0,0,1,0,0,0,1,0)
x7=c(0,0,1,0,0,0,1,0)
x8=c(0,0,0,1,0,0,0,1)
x=rbind(x1,x2,x3,x4,x5,x6,x7,x8)
library(Matrix)
library(MASS)
r=rankMatrix(x)
n=8
# a)
n-r # rank of error space
# b) Estimability of parametric funcitons
s=t(x)%*%x
G=ginv(s)
H=G%*%s
# i)theta2-theta4
l1=c(0,1,0,-1,0,0,0,0)
t(l1)
zapsmall(t(l1)%*%H)
# ii)theta1+theta2+theta3+3theta5
l2=c(1,1,1,0,3,0,0,0)
t(l2)
zapsmall(t(l2)%*%H)

# iii)theta1-theta2
l3=matrix(c(1,-1,0,0,0,0,0,0),8,1)
t(l3)
zapsmall(t(l3)%*%H)
# iv)theta3+2theta5-theta1-2theta8
l4=matrix(c(-1,0,1,0,2,0,0,-2),8,1)
t(l4)
zapsmall(t(l4)%*%H)
# c)
Y=Matrix(c(60.2,74.39,77.88,94.75,81.47,99.34,111.86,127.68))
beta_hat3=G%*%t(x)%*%Y
z1=c(1,0,1,2,3,0,5,6)
z2=c(1,3,1,2,3,4,5,6)
beta_tilda1=beta_hat3+(diag(8)-H)%*%z1
beta_tilda2=beta_hat3+(diag(8)-H)%*%z2
t(l1)%*%beta_hat3
t(l1)%*%beta_tilda1
t(l1)%*%beta_tilda2
# d) Estimate of error variance
y_hat=x%*%beta_hat3
y_hat
e=Y-y_hat
sse=t(e)%*%e
sigma2_hat=sse/(n-r)
var_l1beta_hat3=t(l1)%*%G%*%l1*sigma2_hat

#Que.3
x1=c(1,0,0,0,1,0,0,0)
x2=c(1,0,0,0,0,1,0,0)
x3=c(1,0,0,0,0,0,0,1)
x4=c(0,1,0,0,0,1,0,0)
x5=c(0,0,0,0,0,0,1,0)
x6=c(0,0,0,0,1,0,0,1)
x7=c(0,0,1,0,1,0,0,0)
x8=c(0,0,1,0,0,1,0,0)
x9=c(0,0,1,0,0,0,1,0)
x10=c(0,0,0,1,1,0,0,0)
x11=c(0,0,0,1,0,0,1,0)
x12=c(0,0,0,1,1,0,0,1)
x=rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
x
library(MASS)
library(Matrix)
r=rankMatrix(x)
r[1]
s=t(x)%*%x
G=ginv(s)
H=G%*%s

#rank of error space
n=12
rank_error_space=n-r[1]
rank_estimation_space=r[1]
#checking estimability 
# i)theta1+alpha3
l1=matrix(c(1,0,0,0,0,0,1,0),8,1)
t(l1)
zapsmall(t(l1)%*%H)

# ii)theta1+theta2+theta3-3theta4
l2=matrix(c(1,1,1,-3,0,0,0,0),8,1)
t(l2)
zapsmall(t(l2)%*%H)

# iii)3*theta2+alpha2+alpha3+alpha4
l3=matrix(c(0,3,0,0,0,1,1,1),8,1)
t(l3)
zapsmall(t(l3)%*%H)

# iv)theta1-2*theta4+alpha1
l4=matrix(c(1,0,0,-2,1,0,0,0),8,1)
t(l2)
zapsmall(t(l4)%*%H)

# c) Solution of 
y=c(73,74,71,75,67,72,73,75,68,75,72,75)
beta_hat=G%*%t(x)%*%y
y_hat=x%*%beta_hat
e=y-y_hat
sse=t(e)%*%e
sigma_hat=sse/(n-r[1])
var_l1beta_hat=t(l1)%*%t(G)%*%l1%*%sigma_hat
  