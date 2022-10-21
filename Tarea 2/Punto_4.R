# Julieth Natalia Salazar Vargas 
# Mavelyn Sterling Londono

######################### Parte a) 

### Simular un Ruido Blanco
ruidoBlanco=round(rnorm(200, 0, 0.001), 4);ruidoBlanco
x11()
plot.ts(ruidoBlanco, xlab="Tiempo", ylab="Valores", type ="o")

## Prueba de hipotesis con la prueba Ljung-Box
# Ho : Es un ruido blanco
# H1 : No es un ruido blanco

Box.test(ruidoBlanco)

# dado que p-value > alpha, no se rechaza Ho, por lo que hay evidencia 
# para afirmar que las variables son de un ruido blanco


# Funcion de autocorrelacion
ruidoBlanco_acf <- acf(ruidoBlanco, type = "correlation", plot = T)
ruidoBlanco_acf$acf

# Funcion de autocorrelacion parcial
ruidoBlanco_pcaf<- acf(ruidoBlanco, type = "partial", plot = T)
ruidoBlanco_pcaf$acf

x11()
par(mfrow=c(1,2))
ruidoBlanco_acf <- acf(ruidoBlanco, type = "correlation", plot = T)
ruidoBlanco_pcaf<- acf(ruidoBlanco, type = "partial", plot = T)
########################################################################

######################### Parte b) 
######################### item 1
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

################################################################################################
######################### item 2
############# AR(1) Positivo

ar_1_b1 <- arima.sim(model=list(order = c(1,0,0), ar=0.6), n=200);ar_1_b1

ts.plot(ar_1_b1, xlab="Tiempo", ylab="Valores") # serie de tiempo con AR(1)

# Funcion de autocorrelacion
ar_1_acf_b1 <- acf(ar_1_b1, type = "correlation", plot = T)
ar_1_acf_b1$acf

# Funcion de autocorrelacion parcial
ar_1_pacf_b1<- acf(ar_1_b1, type = "partial", plot = T)
ar_1_pacf_b1$acf

x11()
par(mfrow=c(1,2))
ar_1_acf_b1 <- acf(ar_1_b1, type = "correlation", plot = T, main="AR(1)")
ar_1_pacf_b1<- acf(ar_1_b1, type = "partial", plot = T, main="AR(1)")


############# AR(1) Negativo

ar_1_b2 <- arima.sim(model=list(order = c(1,0,0), ar=-0.6), n=200);ar_1_b2

ts.plot(ar_1_b2,xlab="Tiempo", ylab="Valores") # serie de tiempo con AR(1)


# Funcion de autocorrelacion
ar_1_acf_b2 <- acf(ar_1_b2, type = "correlation", plot = T)
ar_1_acf_b2$acf

# Funcion de autocorrelacion parcial
ar_1_pacf_b2<- acf(ar_1_b2, type = "partial", plot = T)
ar_1_pacf_b2$acf

x11()
par(mfrow=c(1,2))
ar_1_acf_b2 <- acf(ar_1_b2, type = "correlation", plot = T, main="AR(1)")
ar_1_pacf_b2<- acf(ar_1_b2, type = "partial", plot = T, main="AR(1)")


############# AR(3)  Positivos


ar_3_b1 <- arima.sim(model=list(ar=c(0.4,0.02,0.3)), n=200);ar_3_b1

ts.plot(ar_3_b1, ylab= "Valores", xlab="Tiempo") # serie de tiempo con AR(3)

# Funcion de autocorrelacion
ar_3_acf_b1 <- acf(ar_3_b1, type = "correlation", plot = T)
ar_3_acf_b1$acf

# Funcion de autocorrelacion parcial
ar_3_pacf_b1<- acf(ar_3_b1, type = "partial", plot = T, main="AR(3)")
ar_3_pacf_b1$acf

x11()
par(mfrow=c(1,2))
ar_3_acf_b1 <- acf(ar_3_b1, type = "correlation", plot = T, main="AR(3)")
ar_3_pacf_b1<- acf(ar_3_b1, type = "partial", plot = T, main="AR(3)")

############# AR(3) Negativos

ar_3_b2 <- arima.sim(model=list(ar=c(-0.4,-0.02,-0.3)), n=200);ar_3_b2

ts.plot(ar_3_b2, ylab= "Valores", xlab="Tiempo") # serie de tiempo con AR(3)

# Funcion de autocorrelacion
ar_3_acf_b2 <- acf(ar_3_b2, type = "correlation", plot = T)
ar_3_acf_b2$acf

# Funcion de autocorrelacion parcial
ar_3_pacf_b2<- acf(ar_3_b2, type = "partial", plot = T)
ar_3_pacf_b2$acf

x11()
par(mfrow=c(1,2))
ar_3_acf_b2 <- acf(ar_3_b2, type = "correlation", plot = T,  main="AR(3) Negativos")
ar_3_pacf_b2<- acf(ar_3_b2, type = "partial", plot = T,  main="AR(3) Negativos")

############# AR(3) 2 Positivos y  1 Negativo

ar_3_b3 <- arima.sim(model=list(ar=c(-0.4,0.03,0.5)), n=200);ar_3_b3

ts.plot(ar_3_b3,ylab= "Valores", xlab="Tiempo") # serie de tiempo con AR(3)

# Funcion de autocorrelacion
ar_3_acf_b3 <- acf(ar_3_b3, type = "correlation", plot = T)
ar_3_acf_b3$acf

# Funcion de autocorrelacion parcial
ar_3_pacf_b3<- acf(ar_3_b3, type = "partial", plot = T)
ar_3_pacf_b3$acf

x11()
par(mfrow=c(1,2))
ar_3_acf_b3 <- acf(ar_3_b3, type = "correlation", plot = T, main="AR(3)")
ar_3_pacf_b3<- acf(ar_3_b3, type = "partial", plot = T, main="AR(3)")


############# MA(1) Positivo

ma_1_b1 <- arima.sim(model=list(ma=c(0.3)), n=200);ma_1_b1

ts.plot(ma_1_b1,ylab= "Valores", xlab="Tiempo") # serie de tiempo con MA(1)

# Funcion de autocorrelacion
ma_1_acf_b1 <- acf(ma_1_b1, type = "correlation", plot = T)
ma_1_acf_b1$acf

# Funcion de autocorrelacion parcial
ma_1_pacf_b1<- acf(ma_1_b1, type = "partial", plot = T)
ma_1_pacf_b1$acf

x11()
par(mfrow=c(1,2))
ma_1_acf_b1 <- acf(ma_1_b1, type = "correlation", plot = T, main="MA(1)")
ma_1_pacf_b1<- acf(ma_1_b1, type = "partial", plot = T,  , main="MA(1)")


############# MA(1) Negativo

ma_1_b2 <- arima.sim(model=list(ma=c(-0.3)), n=200);ma_1_b2

ts.plot(ma_1_b2, ylab= "Valores", xlab="Tiempo") # serie de tiempo con MA(1)

# Funcion de autocorrelacion
ma_1_acf_b2 <- acf(ma_1_b2, type = "correlation", plot = T)
ma_1_acf_b2$acf

# Funcion de autocorrelacion parcial
ma_1_pacf_b2<- acf(ma_1_b2, type = "partial", plot = T)
ma_1_pacf_b2$acf

x11()
par(mfrow=c(1,2))
ma_1_acf_b2 <- acf(ma_1_b2, type = "correlation", plot = T, main="MA(1)")
ma_1_pacf_b2<- acf(ma_1_b2, type = "partial", plot = T , main="MA(1)")

############# MA(3) Positivos 

ma_3_b1 <- arima.sim(model=list(ma=c(0.4,0.03,0.5)), n=200);ma_3_b1

ts.plot(ma_3_b1, ylab= "Valores", xlab="Tiempo") # serie de tiempo con MA(3)

# Funcion de autocorrelacion
ma_3_acf_b1 <- acf(ma_3_b1, type = "correlation", plot = T)
ma_3_acf_b1$acf

# Funcion de autocorrelacion parcial
ma_3_pacf_b1<- acf(ma_3_b1, type = "partial", plot = T)
ma_3_pacf_b1$acf


x11()
par(mfrow=c(1,2))
ma_3_acf_b1 <- acf(ma_3_b1, type = "correlation", plot = T,  main="MA(3)")
ma_3_pacf_b1<- acf(ma_3_b1, type = "partial", plot = T,  main="MA(3)")

############# MA(3) Negativos 

ma_3_b2 <- arima.sim(model=list(ma=c(-0.4,-0.03,-0.5)), n=200);ma_3_b2

ts.plot(ma_3_b2,ylab= "Valores", xlab="Tiempo") # serie de tiempo con MA(3)

# Funcion de autocorrelacion
ma_3_acf_b2 <- acf(ma_3_b2, type = "correlation", plot = T)
ma_3_acf_b2$acf

# Funcion de autocorrelacion parcial
ma_3_pacf_b2<- acf(ma_3_b2, type = "partial", plot = T)
ma_3_pacf_b2$acf


x11()
par(mfrow=c(1,2))
ma_3_acf_b2 <- acf(ma_3_b2, type = "correlation", plot = T, main="MA(3)")
ma_3_pacf_b2<- acf(ma_3_b2, type = "partial", plot = T, main="MA(3)")

############# MA(3)2 Positivos y 1 Negativos

ma_3_b3 <- arima.sim(model=list(ma=c(-0.4,0.3,0.5)), n=200);ma_3_b3

ts.plot(ma_3_b3,ylab= "Valores", xlab="Tiempo") # serie de tiempo con MA(3)

# Funcion de autocorrelacion
ma_3_acf_b3 <- acf(ma_3_b3, type = "correlation", plot = T)
ma_3_acf_b3$acf

# Funcion de autocorrelacion parcial
ma_3_pacf_b3<- acf(ma_3_b3, type = "partial", plot = T)
ma_3_pacf_b3$acf


x11()
par(mfrow=c(1,2))
ma_3_acf_b3 <- acf(ma_3_b3, type = "correlation", plot = T)
ma_3_pacf_b3<- acf(ma_3_b3, type = "partial", plot = T)


######################### item 3
############# ARMA(1,1) 

arma_1_1<- round(arima.sim(model = list(ar=abs(0.04), ma=abs(0.0001)), n=200),4)
arma_1_1

x11()
ts.plot(arma_1_1,ylab= "Valores", xlab="Tiempo") # serie de tiempo con ARMA(1,1)


# Funcion de autocorrelacion
arma_1_1_C <- acf(arma_1_1, type = "correlation", plot = T, main="ARMA(1,1)")
arma_1_1_C$acf

# Funcion de autocorrelacion parcial
arma_1_1_p<- acf(arma_1_1, type = "partial", plot = T, main="ARMA(1,1)")
arma_1_1_p$acf

x11()
par(mfrow=c(1,2))
arma_1_1_C
arma_1_1_p

x11()
par(mfrow=c(1,2))
arma_1_1 <- acf(arma_1_1, type = "correlation", plot = T , main="ARMA(1,1)")
arma_1_1<- acf(arma_1_1, type = "partial", plot = T)

############# ARIMA(1,1,1) 
arima_1_1_1 <- arima.sim(list(order = c(1,1,1), ar=0.4, ma=0.2), sd=0.1, n = 200)
arima_1_1_1

x11()
ts.plot(arima_1_1_1, ylab= "Valores", xlab="Tiempo") # serie de tiempo con ARIMA(1,1, 1)

# Funcion de autocorrelacion
arima_1_1_1_ACF <- acf(arima_1_1_1, type = "correlation", plot = T)
arima_1_1_1_ACF$acf

# Funcion de autocorrelacion parcial
arma_1_1_1_PACF<- acf(arima_1_1_1, type = "partial", plot = T)
arma_1_1_1_PACF$acf

x11()
par(mfrow=c(1,2))
arima_1_1_1_ACF <- acf(arima_1_1_1, type = "correlation", plot = T, main="ARIMA(1,1,1)")
arma_1_1_1_PACF<- acf(arima_1_1_1, type = "partial", plot = T, main="ARIMA(1,1,1)") 

######################### Parte c) 
######################### item 1

RB_1 <- rnorm(100,0,0) # ruido blanco ~ N(0,0)
serie_0_0<-ts(2*cos((2*pi*1:100/50) + 0.6*pi)+RB_1)

x11()
plot.ts(serie_0_0, xlab="Tiempo", ylab="Valores", type ="o")

# Funcion de autocorrelacion
RB_1_ACF <- acf(serie_0_0, type = "correlation", plot = T)
RB_1_ACF$acf

# Funcion de autocorrelacion parcial
RB_1_PACF<- acf(serie_0_0, type = "partial", plot = T)
RB_1_PACF$acf

x11()
par(mfrow=c(1,2))
RB_1_ACF <- acf(serie_0_0, type = "correlation", plot = T)
RB_1_PACF<- acf(serie_0_0, type = "partial", plot = T)


######################### item 2

RB_2 <- rnorm(100,0,1) # ruido blanco ~ N(0,1)
serie_0_1 <- ts(2*cos((2*pi*1:100/50) + 0.6*pi)+RB_2) # simular la serie

x11()
plot.ts(serie_0_1, xlab="Tiempo", ylab="Valores", type ="o")


# Funcion de autocorrelacion
RB_2_ACF <- acf(serie_0_1, type = "correlation", plot = T)
RB_2_ACF$acf

# Funcion de autocorrelacion parcial
RB_2_PACF<- acf(serie_0_1 ,type = "partial", plot = T)
RB_2_PACF$acf

x11()
par(mfrow=c(1,2))
RB_2_ACF <- acf(serie_0_1, type = "correlation", plot = T)
RB_2_PACF<- acf(serie_0_1, type = "partial", plot = T)

######################### item 3

RB_3 <- rnorm(100,0,5) # ruido blanco ~ N(0,sigma^2=25)

serie_0_25 <-ts(2*cos((2*pi*1:100/50) + 0.6*pi)+RB_3) # simular la serie

x11()
plot.ts(serie_0_25+RB_3, xlab="Tiempo", ylab="Valores", type ="o")


# Funcion de autocorrelacion
RB_3_ACF <- acf(serie_0_25+RB_3, type = "correlation", plot = T)
RB_3_ACF$acf

# Funcion de autocorrelacion parcial
RB_3_PACF<- acf(serie_0_25+RB_3, type = "partial", plot = T)
RB_3_PACF$acf

x11()
par(mfrow=c(1,2))
RB_3_ACF <- acf(serie_0_25+RB_3, type = "correlation", plot = T)
RB_3_PACF<- acf(serie_0_25+RB_3, type = "partial", plot = T)

