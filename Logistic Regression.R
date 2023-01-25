## Que.1
x=c(400,220,490,210,500,270,200,470,480,310,240,490,420,330,280,210,300,470,230,430,460,220,250,200,390)
y=c(0,1,0,1,0,0,1,0,0,1,1,0,0,1,1,1,1,1,0,0,0,1,1,1,0)
fit=glm(y~x,family = 'binomial')
summary(fit)
qchisq(0.05,23,lower.tail = FALSE)
odds=exp(coef(fit))
odds1=exp(cbind(coef(fit),confint(fit)))
plot(fit)       
fit1=glm(y~x+I(x^2),family = 'binomial')
anova(fit,fit1)
anova(fit1)
summary(fit1)

s##Que.3
x1=c(45,40,60,50,55,50,35,65,53,48,37,31,40,75,43,49,37,71,34,27)
x1=x1*1000
x2=c(2,4,3,2,2,5,7,2,2,1,5,7,4,2,9,2,4,1,5,6)
y=c(0,0,1,1,0,1,1,1,0,0,1,1,1,0,1,0,1,0,0,0)
model=glm(y~x1+x2,family = "binomial")
summary(model)
qchisq(0.05,17,lower.tail = FALSE)
