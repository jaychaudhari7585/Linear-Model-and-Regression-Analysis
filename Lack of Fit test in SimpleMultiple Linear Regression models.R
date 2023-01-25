## Que2
y=c(26,24,175,160,163,55,62,100,26,30,70,71)
x1=c(1,1,1.5,1.5,1.5,1,2,0.5,1,1,1,1)
x2=c(1,1,4,4,4,2,5,3,2,2,3,3)
Reduced=lm(y~x1+x2)
summary(Reduced)
Full=lm(y~0+as.factor(x1)+as.factor(x2))
anova(Reduced,Full)

## Que3

y=c(81,89,83,91,79,87,84,90)
x1=c(1,1,2,2,1,1,2,2)
x2=c(150,180,150,180,150,180,150,180)
Reduced=lm(y~x1+x2)
Full=lm(y~0+as.factor(x1)+as.factor(x2))
anova(Reduced,Full)
par(mfrow=c(2,2))
plot(Reduced)
anova(Reduced)
Full$residuals/sqrt(1.1)
