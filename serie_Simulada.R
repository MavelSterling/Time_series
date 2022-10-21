

## Se crea una serie aleatoria con ts
z <- ts(matrix(rnorm(300), 100, 1), start = c(1961, 1), frequency = 12)

class(z)
head(z) # as "matrix"
plot(z)
plot(z, plot.type = "single", lty = 1)

############################### PARTE 1
############# Suavizacion Exponencial Simple
fcast_Simple<- ses(z, h = 20)
summary(fcast_Simple)
plot(fcast_Simple)
############ Suavizacion Exponencial Holt
fcast_holt <- holt(z)
summary(fcast_holt)
plot(fcast_holt)
############ Suavizacion Exponencial HW
fcast_hw <- hw(z,h=48)
plot(z)

############################### PARTE 2
########### serie de tiempo con ggplot2

install.packages("ggfortify")
library(ggfortify)

### Trazamos la serie de tiempo 
autoplot(z, ts.colour = "blue", ts.linetype = "dashed")

### grafico de autocorrelaracion
autoplot(acf(z, plot = FALSE))

######### MODELADO DE LA SERIE

## MODELO ARIMA 
## las funciones ndiffs y nsdiffs, que calculan cada una el 
## n??mero de diferenciaciones regulares y estacionales respectivamente

ndiffs(z)
nsdiffs(z)

# plot de la serie
diff_serieSimulada<-autoplot(diff(z), ts.linetype = "dashed", ts.colour = "darkmagenta")
diff_serieSimulada

# autocorrelacion de la serie
autoplot(acf(diff(z), plot = FALSE))

# mostrar la componente estacional de periodo 12
monthplot(diff(z), col = "midnightblue")

diff_serieSim<-diff(z)
boxplot(diff_serieSim~cycle(diff_serieSim))


# Eliminar la componente estacional
diff_serie_simulada_12<-diff(z, lag = 12)
autoplot(diff_serie_simulada_12, ts.colour = "darkorange4", ts.linetype = "dashed")


# una vez se elimine la componente de tendencia y la estacional
# Se observa que la serie se parece bastante a una serie estacionaria
# con media y varianza constante

# Se aplica un test de estacionariedad como test ADF (Dickey-Fuller)
# y el test KPSS (Kwiatkowski-Pjilips-Schmidt-Shin)
# nivel de significancia alpha=0.05

# Ho : La serie es no estacionaria : tinene raiz unitaria
# H1 : La serie es tacionaria : no tiene raiz unitaria

library(tseries)
adf<-adf.test(diff_serie_simulada_12)
adf$p.value

kpss<-kpss.test(diff_serie_simulada_12)
kpss$p.value


autoplot(acf(diff_serie_simulada_12, plot = FALSE))

autoplot(pacf(diff_serie_simulada_12, plot = FALSE))




