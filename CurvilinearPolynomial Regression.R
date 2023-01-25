## Que.1
y=c(6.3,11.1,20,24,26.1,30,33.8,34,38.1,39.9,42,46.1,53.1,52,52.5,48,42.8,27.8,21.9)
x=c(1,1.5,2,3,4,4.5,5,5.5,6,6.5,7,8,9,10,11,12,13,14,15)
length(y)
length(x)
plot(x,y)
model_1=lm(y~x+I(x^2))
par(mfrow=c(2,2))
plot(model_1)
summary(model_1)
### centering the data
x_1=x-mean(x)
x_2=(x-mean(x))^2
model_2=lm(y~x_1+I(x_2))
summary(model_2)
plot(model_2)
library(car)
vif(model_1)
vif(model_2)
##checking contributions of quadratic term 
model_3=lm(y~x)
anova(model_3,model_2)

## Que.2
y=c(26,24,175,160,163,55,62,100,26,30,70,71)
x1=c(1,1,1.5,1.5,1.5,0.5,1.5,0.5,1,0.5,1,0.5)
x2=c(1,1,4,4,4,2,5,3,1.5,1.5,2.5,2.5)
data=data.frame(y,x1,x2)
plot(data)
model_4=lm(y~x1+x2+I(x1^2)+I(x2^2)+I(x1*x2))
summary(model_4)
red_model_4=lm(y~x1+x2)
anova(red_model_4,model_4)
model_4$residuals
#to detect outlier
par(mfrow=c(1,1))
boxplot(model_4$residuals,outline = T)
r=rstudent(model_4)
r[abs(r)>2]
boxplot(r,outline = T)


##Que.3
y=c(21.6,4,1.8,1,1,0.8,3.8,7.4,4.3,36.2)
x=c(11.15,15.70,18.90,19.40,21.40,21.70,25.30,26.40,26.70,29.10)
plot(x,y)
model=lm(y~x+I(x^2))
anova(model)
par(mfrow=c(2,2))
plot(model)
model_1=lm(y~x)
anova(model_1,model)
