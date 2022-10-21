# Karen Daniela Lopez
# Julieth Natalia Salazar Vargas 
# Mavelyn Sterling 
# Taller de raices unitarias

library(readxl)

datos= read_xlsx("/Users/Mavel/Desktop/datos.xlsx")
attach(datos)
####################################### PUNTO 1


######################## item a

################ serie de PIB

modeloPIB= lm(PIB~Fecha)
summary(modeloPIB)

y = ts(PIB,frequency=4, start=c(2000,01), end = c(2013,03));y
ly = log(y);ly

plot(y)
plot(decompose(y)) # descomposicion de la serie
plot(decompose(diff(log(y)))) # descomposicion de la serie


library(ggfortify)
autoplot(y, ts.colour = "blue", ts.linetype = "dashed", ylim=c(69000, 110000))
autoplot(acf(y, plot = FALSE))
autoplot(stl(y, s.window = "periodic"), ts.colour = "blue")
ggAcf(PIB)

##########  lnPIB
library(forecast)
library(urca)
library(fUnitRoots)
library(tseries)
library(ggplot2)


ggAcf(ly)
ggtsdisplay(ly, main="lnPIB")

# Modelo AR(p) con tendencia lineal ("trend")
df_tend0 = ur.df(y = ly, lags = 0, type = "trend" );df_tend0
df_tend = ur.df(y = ly, lags = 3, type = "trend" );df_tend
summary(df_tend)
summary(df_tend0)

# Ho: La serie no es estacionaria (∏ = 0) » Ø = 1
# Ha: La serie es estacionaria (∏ < 0) » |Ø| < 1
adf.test(ly, k=0) # Dickey-Fuller aumentada (ADF)
adf.test(ly, k=3) # Dickey-Fuller aumentada (ADF)

# Ho: La serie es estacionaria
# Ha: La serie no es estacionaria
kpss.test(ly, null = "Trend") # KPSS


########## ∆lnPIB
df_tend_diff = ur.df(diff(ly), lags = 0, type = "trend" )
df_tend_diff3 = ur.df(diff(ly), lags = 3, type = "trend" )

summary(df_tend_diff)
summary(df_tend_diff3)

library(ggplot2)
ggAcf(diff(ly))
ggtsdisplay(diff(ly), main="∆lnPIB", theme = theme_minimal())

# Ho: La serie no es estacionaria (∏ = 0) » Ø = 1
# Ha: La serie es estacionaria (∏ < 0) » |Ø| < 1
adf.test(diff(ly), k=0) # Dickey-Fuller aumentada (ADF)
adf.test(diff(ly), k=3) # Dickey-Fuller aumentada (ADF)


# Ho: La serie es estacionaria
# Ha: La serie no es estacionaria
kpss.test(diff(ly), null = "Trend") # KPSS

################ serie de EXP
z = ts(EXP,frequency=4, start=c(2000,01), end = c(2013,03))
lz = log(z)

library(ggfortify)
autoplot(z, ts.colour = "blue", ts.linetype = "dashed")
autoplot(acf(z, plot = FALSE))
autoplot(stl(z, s.window = "periodic"), ts.colour = "blue")


##########  lnEXP
ggtsdisplay(lz, main="lnEXP")

# Modelo AR(p) con tendencia lineal ("trend")
df_tend0_exp = ur.df(lz, lags = 0, type = "trend" )
df_tend1_exp = ur.df(lz, lags = 3, type = "trend" )
summary(df_tend0_exp)
summary(df_tend1_exp)



# Ho: La serie no es estacionaria (∏ = 0) » Ø = 1
# Ha: La serie es estacionaria (∏ < 0) » |Ø| < 1
adf.test(lz, k=0) # Dickey-Fuller aumentada (ADF)
adf.test(lz, k=3) # Dickey-Fuller aumentada (ADF)


# Ho: La serie es estacionaria
# Ha: La serie no es estacionaria
kpss.test(lz, null = "Trend") # KPSS


########## ∆lnEXP

ggtsdisplay(lz, main="∆lnEXP")


# Modelo AR(p) con tendencia lineal ("trend")
df_tend0_diff = ur.df(diff(lz), lags = 0, type = "trend" )
df_tend1_diff = ur.df(diff(lz), lags = 3, type = "trend" )
summary(df_tend0_diff)
summary(df_tend1_diff)


# Ho: La serie no es estacionaria (∏ = 0) » Ø = 1
# Ha: La serie es estacionaria (∏ < 0) » |Ø| < 1
adf.test(diff(lz), k=0) # Dickey-Fuller aumentada (ADF)
adf.test(diff(lz), k=3) # Dickey-Fuller aumentada (ADF)


# Ho: La serie es estacionaria
# Ha: La serie no es estacionaria
kpss.test(diff(lz), null = "Trend") # KPSS




## estrategia de regresiones para la df aumentada
library(dynlm)

dly<- diff(log(y))

reg0 = dynlm(dly ~ L(ly,1))
reg1 = dynlm(dly ~ L(ly,1) + L(dly, 1))
reg2 = dynlm(dly ~ L(ly,1) + L(dly, 1) + L(dly, 2))
reg3 = dynlm(dly ~ L(ly,1) + L(dly, 1) + L(dly, 2) + L(dly, 3))

(c(AIC(reg0),AIC(reg1),AIC(reg2),AIC(reg3)))

# con tendencia
reg_tend0 = dynlm(dly ~ trend(ly) + L(ly,1))
reg_tend1 = dynlm(dly ~ trend(ly) + L(ly,1) + L(dly, 1))
reg_tend2 = dynlm(dly ~ trend(ly) + L(ly,1) + L(dly, 1) + L(dly, 2))
reg_tend3 = dynlm(dly ~ trend(ly) + L(ly,1) + L(dly, 1) + L(dly, 2)+ L(dly, 3))
(c(AIC(reg_tend0),AIC(reg_tend1),AIC(reg_tend2),AIC(reg_tend3)))



library(ggpmisc)
ggplot(ly, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%Y")+
  theme_minimal()



ggplot(diff(ly), as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%Y")+
  theme_minimal()


ggplot(lz, as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%Y")+
  theme_minimal()

ggplot(diff(lz), as.numeric = FALSE) + geom_line() + 
  stat_peaks(colour = "red") +
  stat_peaks(geom = "text", colour = "red", 
             vjust = -0.5, x.label.fmt = "%Y") +
  stat_valleys(colour = "blue") +
  stat_valleys(geom = "text", colour = "blue", angle = 45,
               vjust = 1.5, hjust = 1,  x.label.fmt = "%Y")+
  theme_minimal()

####################################### PUNTO 2


z<-0
tau<-0
for(j in 1:10000){
  z[1]<-0
  for(i in 2:50){
    z[i]<-z[i-1]+rnorm(1,0,1)}
  
  difZ<-diff(z)
  zt_1<-z[1:49]
  modelo<-summary(lm(difZ~zt_1-1))
  pi<-modelo$coefficients[1]
  stderror<-modelo$coefficients[2]
  tau[j]<-pi/(stderror)
} 
tau  
quantile(tau, prob=c(0.01,0.05,0.1))

library(ggplot2)
tau<-as.data.frame(tau)
h1<-ggplot(data = tau,
           aes(x = tau, fill=cut)) +
  geom_histogram(fill = "steelblue", color="black")+
  theme_minimal()+
  xlab("Tau") + 
  ylab("Frecuencia")+ 
  theme (axis.text.x = element_text(size=rel(1.3)),
         axis.text.y = element_text(size=rel(1.3)))+
  theme (axis.title = element_text(size=rel(1.7)));h1

tstudent<-rt(10000,48)
tstudent<-as.data.frame(tstudent)
h2<-ggplot(data = tstudent,
           aes(x = tstudent, fill=cut)) +
  geom_histogram(fill = "steelblue", color="black")+
  theme_minimal()+
  xlab("T-Student") + 
  ylab("Frecuencia")+ 
  theme (axis.text.x = element_text(size=rel(1.3)),
         axis.text.y = element_text(size=rel(1.3)))+
  theme (axis.title = element_text(size=rel(1.7)));h2


library(gridExtra)
grid.arrange(h1, h2, ncol=2, nrow = 1)


plot(tau)









####################################### PUNTO 5


library(rms)

wiener2 = function(nobs) {
 
   e = rnorm(nobs) #Numero de la distribucion normal para los errores
  
  # Se crean los errores con tendencia
  e1 = e - mean(e)
  e2 = residuals(ols(e~seq(1,nobs))) # los residuales del modelo de regresion (OLS)
  
  # Se simula el movimiento Browniano 
  y1 = cumsum(e1) # suma acumulativa de e1
  y2 = cumsum(e2) # suma acumulativa de e2
  
  intW2.1 = nobs^(-2) * sum(y1^2) 
  intW2.2 = nobs^(-2) * sum(y2^2) 
  
  ans = list(intW2.1=intW2.1, intW2.2=intW2.2) # se crea una lista con los resultados
  ans }
#
##### simulacion de KPSS

 nobs = 50            # numero de obervaciones
 nsim = 10000         # numero de simulaciones
 
 KPSS1 = rep(0,nsim)
 KPSS2 = rep(0,nsim)
 for (i in 1:nsim) {
  BN.moments = wiener2(nobs)
  KPSS1[i] = BN.moments$intW2.1
  KPSS2[i] = BN.moments$intW2.2
 }
 
 KPSS1
 KPSS2
 
 library(fUnitRoots)
tabla1= quantile(KPSS1, probs=c(0.90,0.925,0.95,0.975,0.99)) # intercepto
tabla1
tabla2= quantile(KPSS2, probs=c(0.90,0.925,0.95,0.975,0.99)) # intercepto y tendencia
 tabla2
 
 
 #####
 library(ggplot2)
 
 library(gridExtra)
 
 KPSS1<-as.data.frame(KPSS1)
 h3<-ggplot(data = KPSS1,
            aes(x = KPSS1, fill=cut)) +
   geom_histogram(fill = "steelblue", color="black")+
   theme_minimal()+
   xlab("KPSS") + 
   ylab("Frecuencia")+ 
   theme (axis.text.x = element_text(size=rel(1.3)),
          axis.text.y = element_text(size=rel(1.3)))+
   theme (axis.title = element_text(size=rel(1.7)));h3
 
 KPSS2<-as.data.frame(KPSS2)
 h4<-ggplot(data = KPSS2,
            aes(x = KPSS2, fill=cut)) +
   geom_histogram(fill = "steelblue", color="black")+
   theme_minimal()+
   xlab("KPSS") + 
   ylab("Frecuencia")+ 
   theme (axis.text.x = element_text(size=rel(1.3)),
          axis.text.y = element_text(size=rel(1.3)))+
   theme (axis.title = element_text(size=rel(1.7)));h4
 
 grid.arrange(h3, h4, ncol=2, nrow = 1)
 
 

 
 
####################################### PUNTO 6
################### PRUEBA Phillips - Perron (1988)

checkresiduals(fit3)

##########  lnPIB
PP.test(ly)

PP.test(ly, lshort = TRUE)

pp.test(ly, type = "Z(t_alpha)") # con tendencia
pp.test(ly, type = "Z(alpha)") # 

############## PIB
summary(ur.pp(ly, type =  "Z-tau", model ="constant", use.lag = 0))
summary(ur.pp(ly, type =  "Z-tau", model = "trend", use.lag = 0))

summary(ur.pp(ly, type =  "Z-tau", model ="constant", use.lag = 3))
summary(ur.pp(ly, type =  "Z-tau", model = "trend", use.lag = 3))


summary(ur.pp(diff(ly), type =  "Z-tau", model ="constant", use.lag = 0))
summary(ur.pp(diff(ly), type =  "Z-tau", model = "trend", use.lag = 0))

summary(ur.pp(diff(ly), type =  "Z-tau", model ="constant", use.lag = 3))
summary(ur.pp(diff(ly), type =  "Z-tau", model = "trend", use.lag = 3))

############## Exportaciones
summary(ur.pp(lz, type =  "Z-tau", model ="constant", use.lag = 0))
summary(ur.pp(lz, type =  "Z-tau", model = "trend", use.lag = 0))

summary(ur.pp(lz, type =  "Z-tau", model ="constant", use.lag = 3))
summary(ur.pp(lz, type =  "Z-tau", model = "trend", use.lag = 3))


summary(ur.pp(diff(lz), type =  "Z-tau", model ="constant", use.lag = 0))
summary(ur.pp(diff(lz), type =  "Z-tau", model = "trend", use.lag = 0))

summary(ur.pp(diff(lz), type =  "Z-tau", model ="constant", use.lag = 3))
summary(ur.pp(diff(lz), type =  "Z-tau", model = "trend", use.lag = 3))

