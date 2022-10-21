############# AR(2) 

ar_2_a <- arima.sim(model=list(ar=c(0.2,0.3)), n=200);ar_2_a

ts.plot(ar_2_a) # serie de tiempo con AR(2)

# Funcion de autocorrelacion
ar_2_acf_a <- acf(ar_2_a, type = "correlation", plot = T)
ar_2_acf_a$acf

# Funcion de autocorrelacion parcial
ar_2_pacf_a<- acf(ar_2_a, type = "partial", plot = T)
ar_2_pacf_a$acf

x11()
par(mfrow=c(1,2))
ar_2_acf_a <- acf(ar_2_a, type = "correlation", plot = T)
ar_2_pacf_a<- acf(ar_2_a, type = "partial", plot = T)

############# MA(2) 

ma_2_a <- arima.sim(model=list(ma=c(0.2,0.3)), n=200)
ma_2_a


ts.plot(ma_2_a) # serie de tiempo con MA(2)

# Funcion de autocorrelacion
ma_2_acf_a<- acf(ma_2_a,type = "correlation", plot = T)
ma_2_acf_a$acf

# Funcion de autocorrelacion parcial
ma_2_pacf_a<- acf(ma_2_a, type = "partial", plot = T)
ma_2_pacf_a$acf


x11()
par(mfrow=c(1,2))
ma_2_acf_a<- acf(ma_2_a,type = "correlation", plot = T)
ma_2_pacf_a<- acf(ma_2_a, type = "partial", plot = T)


######################### item 2
############# AR(2) Positivos

ar_2_b1 <- arima.sim(model=list(ar=c(0.4,0.3)), n=200);ar_2_b1

ts.plot(ar_2_b1) # serie de tiempo con AR(2)

# Funcion de autocorrelacion
ar_2_acf_b1 <- acf(ar_2_b1, type = "correlation", plot = T)
ar_2_acf_b1$acf

# Funcion de autocorrelacion parcial
ar_2_pacf_b1<- acf(ar_2_b1, type = "partial", plot = T)
ar_2_pacf_b1$acf

x11()
par(mfrow=c(1,2))
ar_2_acf_b1 <- acf(ar_2_b1, type = "correlation", plot = T)
ar_2_pacf_b1<- acf(ar_2_b1, type = "partial", plot = T)


############# AR(2) Negativos

ar_2_b2 <- arima.sim(model=list(ar=c(-0.4,-0.3)), n=200);ar_2_b2

ts.plot(ar_2_b2) # serie de tiempo con AR(2)

# Funcion de autocorrelacion
ar_2_acf_b2 <- acf(ar_2_b2, type = "correlation", plot = T)
ar_2_acf_b2$acf

# Funcion de autocorrelacion parcial
ar_2_pacf_b2<- acf(ar_2_b2, type = "partial", plot = T)
ar_2_pacf_b2$acf

x11()
par(mfrow=c(1,2))
ar_2_acf_b2 <- acf(ar_2_b2, type = "correlation", plot = T)
ar_2_pacf_b2<- acf(ar_2_b2, type = "partial", plot = T)


############# AR(2) Positivo y Negativo

ar_2_b3 <- arima.sim(model=list(ar=c(-0.4,0.3)), n=200);ar_2_b3

ts.plot(ar_2_b3) # serie de tiempo con AR(2)

# Funcion de autocorrelacion
ar_2_acf_b3 <- acf(ar_2_b3, type = "correlation", plot = T)
ar_2_acf_b3$acf

# Funcion de autocorrelacion parcial
ar_2_pacf_b3<- acf(ar_2_b3, type = "partial", plot = T)
ar_2_pacf_b3$acf

x11()
par(mfrow=c(1,2))
ar_2_acf_b3 <- acf(ar_2_b3, type = "correlation", plot = T)
ar_2_pacf_b3<- acf(ar_2_b3, type = "partial", plot = T)


############# MA(2) Positivos

ma_2_b1 <- arima.sim(model=list(ma=c(0.4,0.3)), n=200);ma_2_b1

ts.plot(ma_2_b1) # serie de tiempo con MA(2)

# Funcion de autocorrelacion
ma_2_acf_b1 <- acf(ma_2_b1, type = "correlation", plot = T)
ma_2_acf_b1$acf

# Funcion de autocorrelacion parcial
ma_2_pacf_b1<- acf(ma_2_b1, type = "partial", plot = T)
ma_2_pacf_b1$acf

x11()
par(mfrow=c(1,2))
ma_2_acf_b1 <- acf(ma_2_b1, type = "correlation", plot = T)
ma_2_pacf_b1<- acf(ma_2_b1, type = "partial", plot = T)


############# MA(2) Negativos

ma_2_b2 <- arima.sim(model=list(ma=c(-0.4,-0.3)), n=200);ma_2_b2

ts.plot(ma_2_b2) # serie de tiempo con MA(2)

# Funcion de autocorrelacion
ma_2_acf_b2 <- acf(ma_2_b2, type = "correlation", plot = T)
ma_2_acf_b2$acf

# Funcion de autocorrelacion parcial
ma_2_pacf_b2<- acf(ma_2_b2, type = "partial", plot = T)
ma_2_pacf_b2$acf

x11()
par(mfrow=c(1,2))
ma_2_acf_b2 <- acf(ma_2_b2, type = "correlation", plot = T)
ma_2_pacf_b2<- acf(ma_2_b2, type = "partial", plot = T)


############# MA(2) Positivo y Negativo

ma_2_b3 <- arima.sim(model=list(ma=c(-0.4,0.3)), n=200);ma_2_b3

ts.plot(ma_2_b3) # serie de tiempo con MA(2)

# Funcion de autocorrelacion
ma_2_acf_b3 <- acf(ma_2_b3, type = "correlation", plot = T)
ma_2_acf_b3$acf

# Funcion de autocorrelacion parcial
ma_2_pacf_b3<- acf(ma_2_b3, type = "partial", plot = T)
ma_2_pacf_b3$acf


x11()
par(mfrow=c(1,2))
ma_2_acf_b3 <- acf(ma_2_b3, type = "correlation", plot = T)
ma_2_pacf_b3<- acf(ma_2_b3, type = "partial", plot = T)

