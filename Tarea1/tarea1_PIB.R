##   SERIES DE TIEMPO
##   TAREA 1

install.packages("readxl")
library(readxl)

Datos=read_xlsx("Mavel/Desktop/PIB.xlsx")
seriePIB= ts(Datos$PIB, start = c(1960,1))

# grafico de la serie
autoplot(seriePIB, ts.colour = "blue", ts.linetype = "dashed")

############################### PARTE 1
############# Suavizacion Exponencial Simple
install.packages("forecast")
library(forecast)

fcast_seriePIB<- ses(seriePIB, h = 12)
summary(fcast_seriePIB)
plot(fcast_seriePIB, ts.colour = "blue", ts.linetype = "dashed")

############ Suavizacion Exponencial Holt
fcast <- holt(seriePIB)
summary(fcast)
plot(fcast, ts.colour = "blue", ts.linetype = "dashed")

############ Suavizacion Exponencial HW
fcastHW <- hw(seriePIB, h=2*frequency(Datos$PIB), seasonal = c("additive", "multiplicative"))
summary(fcastHW)
autoplot(fcastHW, ts.colour = "blue", ts.linetype = "dashed")


############################### PARTE 2
########### serie de tiempo con ggplot2

# autocorrelacion de la serie
autoplot(acf(diff(seriePIB), plot = FALSE))

