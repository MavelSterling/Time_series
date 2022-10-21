plot(ip,main="Indice de precios al productor",xlab="t",ylab=expression(Z[t]))

library(MASS)
bc<-boxcox(ip~1)
lam<-bc$x[which.max(bc$y)]
lam

lip<-log(ip)
dlip<-diff(lip)
plot(dlip,main="Diferenciación del logaritmo\n
Indice de precios al productor",xlab="t",ylab=expression(paste(Delta,Z[t])))

par(mfrow=c(2,1))
acf(diff(tdes),lag.max=60)
acf(diff(tdes),lag.max=60,type="partial")

dtd<-diff(tdes,1,12)

m1<-arima(dtd,order=c(2,0,1),seasonal=list(order=c(2,0,0)),fix=c(NA,0,NA,NA,NA,NA))
library(lmtest)
coeftest(m1)
library(forecast)
autoplot(m1)

par(mfrow=c(1,2))
acf(dlip,main="FAC retornos IPP")
acf(dlip,main="FACP retornos IPP",type="partial")

mA1<-arima(dlip,order=c(1,0,1))
library(lmtest)
coeftest(mA1)
autoplot(mA1)


mA2<-arima(dlip,order=c(1,0,0))
coeftest(mA2)
autoplot(mA2)

mA3<-arima(dlip,order=c(0,0,1))
coeftest(mA3)
autoplot(mA3)

resid1<-mA2$residuals
resid2<-mA3$residuals


par(mfrow=c(1,2))
plot(resid1)
plot(resid2)


layout(matrix(c(1,2,3,4),ncol=2))
hist(resid1,freq = F,main = "Residuales modelo AR",col="deepskyblue",xlab = "")
lines(seq(-3,3,0.0001),dnorm(seq(-3,3,0.0001),0,sd(resid1)),col="darkorchid")
qqnorm(resid1,main = "Normal Q-Q Plot - Residuales modelo AR")
qqline(resid1)
hist(resid2,freq = F,main = "Residuales modelo MA",col="deepskyblue",xlab = "")
lines(seq(-3,3,0.0001),dnorm(seq(-3,3,0.0001),0,sd(resid2)),,col="darkorchid")
qqnorm(resid2,main = "Normal Q-Q Plot - Residuales modelo MA")
qqline(resid2)

ks.test(resid1,"pnorm",0,sd(resid1))

mA2$coef
mA2lm<-lm(dlip[1:(length(dlip)-1)]~dlip[2:length(dlip)])
mA2lm$coefficients

bptest(mA2lm)


bplm1<-lm(mA2lm$residuals^2~dlip[2:length(dlip)])
r2bp<-summary(bplm1)$r.squared
bpstat<-length(mA2lm$residuals)*r2bp
vp.bp1<-1-pchisq(bpstat,1)

length(resid1)
length(mA2lm$residuals)

resid11<-resid1-mA2$coef[2]/(1-mA2$coef[1])
bpl<-lm(resid11[2:length(dlip)]^2~dlip[1:(length(dlip)-1)])
pbt<-summary(bpl)$r.squared;pbt

bps<-(length(resid1)-1)*pbt;bps

vpbp<-1-pchisq(bps,1);vpbp

mA3lm<-lm(dlip[2:length(dlip)]~resid1[1:(length(dlip)-1)])
mA3lm$coefficients
mA3$coef

bptest(mA3lm)

bpl2<-lm(resid2[2:length(dlip)]^2~resid1[1:(length(dlip)-1)])

pbt1<-summary(bpl2)$r.squared;pbt1
bps2<-(length(resid2)-1)*pbt1;bps2
vpbpm2<-1-pchisq(bps2,1);vpbpm2


par(mfrow=c(1,2))
plot(resid1^2)
plot(resid2^2)

acf(resid1^2)
acf(resid2^2)

library(aTSA)
arch.test(mA2)

library(MTS)
archTest(resid1,lag=5)

Box.test(resid1^2,type="Ljung-Box",lag = 5)









