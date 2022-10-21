# Julieth Natalia Salazar Vargas 
# Mavelyn Sterling Londono


##########################################
############       item 1    ############# 
##########################################

### Simular un Ruido Blanco
ruidoBlanco=round(rnorm(200, 0, 0.001), 4)
ruidoBlanco_acf <- acf(ruidoBlanco, type = "correlation") # Funcion de autocorrelacion
ruidoBlanco_pcaf<- acf(ruidoBlanco, type = "partial") # Funcion de autocorrelacion parcial

x11()
par(mar=c(5,5,2,1))
plot(ruidoBlanco, type="l", cex.lab=1.5,cex.axis=1.35,
     xlab="Tiempo", ylab="Valores RB")

x11()
par(mfrow=c(1,2),mar=c(5,5,2,1))
plot(ruidoBlanco_acf,cex.lab=1.5,cex.axis=1.35,main="")
plot(ruidoBlanco_pcaf,cex.lab=1.5,cex.axis=1.35,main="")

Box.test(ruidoBlanco) 



##########################################
############       item 2    ############# 
##########################################


############# AR(1) Positivo ############# 

ar_1_b1 <- arima.sim(model=list(order = c(1,0,0), ar=0.6), n=200) # serie de tiempo
ar_1_acf_b1 <- acf(ar_1_b1, type = "correlation") # Funcion de autocorrelacion
ar_1_pacf_b1<- acf(ar_1_b1, type = "partial") # Funcion de autocorrelacion parcial

############# AR(1) Negativo ############# 

ar_1_b2 <- arima.sim(model=list(order = c(1,0,0), ar=-0.6), n=200)
ar_1_acf_b2 <- acf(ar_1_b2, type = "correlation") # Funcion de autocorrelacion
ar_1_pacf_b2<- acf(ar_1_b2, type = "partial") # Funcion de autocorrelacion parcial

########### GRAFICO AR(1) POS NEG ###########
x11()
par(mfrow=c(2,3),mar=c(5,5,5,1),cex.main=1.8, cex.lab=1.7,cex.axis=1.5)
plot(ar_1_b1, type="l", xlab="Tiempo", ylab="Valores", main="AR(1) p=0.6") 
plot(ar_1_acf_b1, main="ACF AR(1) p=0.6")
plot(ar_1_pacf_b1, main="PACF AR(1) p=0.6")
plot(ar_1_b2, type="l", xlab="Tiempo", ylab="Valores", main="AR(1) p= - 0.6")
plot(ar_1_acf_b2, main="ACF AR(1) p= - 0.6")
plot(ar_1_pacf_b2, main="PACF AR(1) p= - 0.6")



############# AR(3)  Positivos ############# 

ar_3_b1 <- arima.sim(model=list(ar=c(0.4,0.02,0.3)), n=200)# serie de tiempo con AR(3)
ar_3_acf_b1 <- acf(ar_3_b1, type = "correlation") # Funcion de autocorrelacion
ar_3_pacf_b1<- acf(ar_3_b1, type = "partial") # Funcion de autocorrelacion parcial

############# AR(3) Negativos #############

ar_3_b2 <- arima.sim(model=list(ar=c(-0.4,-0.02,-0.3)), n=200)# serie de tiempo con AR(3)
ar_3_acf_b2 <- acf(ar_3_b2, type = "correlation") # Funcion de autocorrelacion
ar_3_pacf_b2<- acf(ar_3_b2, type = "partial") # Funcion de autocorrelacion parcial

########### GRAFICO AR(3) POS NEG ###########
x11()
par(mfrow=c(2,3),mar=c(5,5,5,1),cex.main=1.8, cex.lab=1.7,cex.axis=1.5)
plot(ar_3_b1,type="l", xlab="Tiempo", ylab="Valores", main="AR(3) p=(0.4,0.02,0.3)")
plot(ar_3_acf_b1, main="ACF AR(3) p=(0.4,0.02,0.3)")
plot(ar_3_pacf_b1, main="PACF AR(3) p=(0.4,0.02,0.3)")
plot(ar_3_b2, type="l", xlab="Tiempo", ylab="Valores", main="AR(3) p=(-0.4,-0.02,-0.3)")
plot(ar_3_acf_b2, main="ACF AR(3) p=(-0.4,-0.02,-0.3)")
plot(ar_3_pacf_b2, main="PACF AR(3) p=(-0.4,-0.02,-0.3)")



############# MA(1) Positivo ############# 

ma_1_b1 <- arima.sim(model=list(ma=c(0.3)), n=200) # serie de tiempo con MA(1)
ma_1_acf_b1 <- acf(ma_1_b1, type = "correlation")# Funcion de autocorrelacion
ma_1_pacf_b1<- acf(ma_1_b1, type = "partial")# Funcion de autocorrelacion parcial

############# MA(1) Negativo ############# 

ma_1_b2 <- arima.sim(model=list(ma=c(-0.3)), n=200) # serie de tiempo con MA(1)
ma_1_acf_b2 <- acf(ma_1_b2, type = "correlation") # Funcion de autocorrelacion
ma_1_pacf_b2<- acf(ma_1_b2, type = "partial") # Funcion de autocorrelacion parcial

########### GRAFICO MA(1) POS NEG ###########
x11()
par(mfrow=c(2,3),mar=c(5,5,5,1),cex.main=1.8, cex.lab=1.7,cex.axis=1.5)
plot(ma_1_b1, type="l", xlab="Tiempo", ylab="Valores", main="MA(1) p=0.3")
plot(ma_1_acf_b1, main="ACF MA(1) p=0.3")
plot(ma_1_pacf_b1, main="PACF MA(1) p=0.3")
plot(ma_1_b2,type="l", xlab="Tiempo", ylab="Valores", main="MA(1) p= - 0.3") 
plot(ma_1_acf_b2, main="ACF MA(1) p= - 0.3")
plot(ma_1_pacf_b2, main="PACF MA(1) p= - 0.3")


############# MA(3) Positivos ############# 

ma_3_b1 <- arima.sim(model=list(ma=c(0.4,0.03,0.5)), n=200) # serie de tiempo con MA(3)
ma_3_acf_b1 <- acf(ma_3_b1, type = "correlation") # Funcion de autocorrelacion
ma_3_pacf_b1<- acf(ma_3_b1, type = "partial") # Funcion de autocorrelacion parcial

############# MA(3) Negativos ############# 

ma_3_b2 <- arima.sim(model=list(ma=c(-0.4,-0.03,-0.5)), n=200);# serie de tiempo con MA(3)
ma_3_acf_b2 <- acf(ma_3_b2, type = "correlation") # Funcion de autocorrelacion
ma_3_pacf_b2<- acf(ma_3_b2, type = "partial") # Funcion de autocorrelacion parcial

########### GRAFICO MA(3) POS NEG ###########
x11()
par(mfrow=c(2,3),mar=c(5,5,5,1),cex.main=1.8, cex.lab=1.7,cex.axis=1.5)
plot(ma_3_b1, type="l", xlab="Tiempo", ylab="Valores", main="MA(3) p=(0.4,0.03,0.5)")
plot(ma_3_acf_b1, main="ACF MA(3) p=(0.4,0.03,0.5)")
plot(ma_3_pacf_b1, main="PACF MA(3) p=(0.4,0.03,0.5)")
plot(ma_3_b2,  type="l", xlab="Tiempo", ylab="Valores", main="MA(3) p=(-0.4,-0.03,-0.5)")
plot(ma_3_acf_b2, main="ACF MA(3) p=(-0.4,-0.03,-0.5)")
plot(ma_3_pacf_b2, main="PACF MA(3) p=(-0.4,-0.03,-0.5)")



############# ARMA(1,1) #############

arma_1_1<- round(arima.sim(model = list(ar=abs(0.04), ma=abs(0.0001)), n=200),4)  # serie de tiempo con ARMA(1,1)
arma_1_1_C <- acf(arma_1_1, type = "correlation") # Funcion de autocorrelacion
arma_1_1_p<- acf(arma_1_1, type = "partial") # Funcion de autocorrelacion parcial

########### GRAFICO ARMA(1,1) #############
x11()
par(mfrow=c(1,3),mar=c(5,5,5,1),cex.main=1.8, cex.lab=1.7,cex.axis=1.5)
plot(arma_1_1, type="l", xlab="Tiempo", ylab="Valores", main="ARMA(1,1) p=(ar=0.04,ma=0.0001)")
plot(arma_1_1_C, main="ARMA(1,1) p=(ar=0.04,ma=0.0001)")
plot(arma_1_1_p, main="ARMA(1,1) p=(ar=0.04,ma=0.0001)")



############# ARIMA(1,1,1) #############
arima_1_1_1 <- arima.sim(list(order = c(1,1,1), ar=0.4, ma=0.2), sd=0.1, n = 200) # serie de tiempo con ARIMA(1,1, 1)
arima_1_1_1_ACF <- acf(arima_1_1_1, type = "correlation") # Funcion de autocorrelacion
arma_1_1_1_PACF<- acf(arima_1_1_1, type = "partial") # Funcion de autocorrelacion parcial

########### GRAFICO ARMIMA(1,1,1) #############
x11()
par(mfrow=c(1,3),mar=c(5,5,5,1),cex.main=1.6, cex.lab=1.7,cex.axis=1.5)
plot(arima_1_1_1, type="l", xlab="Tiempo", ylab="Valores", main="ARIMA(1,1,1) p=(ar=0.4,ma=0.2,desv=0.1)")
plot(arima_1_1_1_ACF, main="ACF ARIMA(1,1,1) p=(ar=0.4,ma=0.2,desv=0.1)")
plot(arma_1_1_1_PACF, main="PACF ARIMA(1,1,1) p=(ar=0.4,ma=0.2,desv=0.1)")


##########################################
############       item 2    ############# 
##########################################


RB_1 <- rnorm(300,0,0) # ruido blanco ~ N(0,0)
RB_2 <- rnorm(300,0,1) # ruido blanco ~ N(0,1)
RB_3 <- rnorm(300,0,5) # ruido blanco ~ N(0,sigma^2=25)

serie_0_0<-ts(2*cos((2*pi*1:100/50) + 0.6*pi)+RB_1) # simular la serie
serie_0_1 <- ts(2*cos((2*pi*1:100/50) + 0.6*pi)+RB_2) # simular la serie
serie_0_25 <-ts(2*cos((2*pi*1:100/50) + 0.6*pi)+RB_3) # simular la serie

RB_1_ACF <- acf(serie_0_0, type = "correlation")# Funcion de autocorrelacion
RB_2_ACF <- acf(serie_0_1, type = "correlation") # Funcion de autocorrelacion
RB_3_ACF <- acf(serie_0_25+RB_3, type = "correlation") # Funcion de autocorrelacion

RB_1_PACF<- acf(serie_0_0, type = "partial")# Funcion de autocorrelacion parcial
RB_2_PACF<- acf(serie_0_1 ,type = "partial") # Funcion de autocorrelacion parcial
RB_3_PACF<- acf(serie_0_25+RB_3, type = "partial") # Funcion de autocorrelacion parcial


x11()
par(mfrow=c(3,1),mar=c(5,5,2,1))
plot(serie_0_0, cex.lab=1.5,cex.axis=1.35, type="l",
     xlab="Tiempo", ylab="Valores", main="N(0,0)") 
plot(serie_0_1, cex.lab=1.5,cex.axis=1.35, type="l",
     xlab="Tiempo", ylab="Valores", main="N(0,1)") 
plot(serie_0_25, cex.lab=1.5,cex.axis=1.35, type="l",
     xlab="Tiempo", ylab="Valores", main="N(0,25)") 

x11()
par(mfrow=c(1,3),mar=c(5,5,4,1))
plot(RB_1_ACF, cex.lab=1.5, cex.axis=1.35,main="RB~N(0,0)")
plot(RB_2_ACF, cex.lab=1.5, cex.axis=1.35,main="RB~N(0,1)")
plot(RB_3_ACF, cex.lab=1.5, cex.axis=1.35,main="RB~N(0,25)")

x11()
par(mfrow=c(1,3),mar=c(5,5,4,1))
plot(RB_1_PACF, cex.lab=1.5, cex.axis=1.35,main="RB~N(0,0)")
plot(RB_2_PACF, cex.lab=1.5, cex.axis=1.35,main="RB~N(0,1)")
plot(RB_3_PACF, cex.lab=1.5, cex.axis=1.35,main="RB~N(0,25)")

