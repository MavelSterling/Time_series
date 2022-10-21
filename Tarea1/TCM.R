

library(readxl)

Datos=read_xlsx("/Users/Mavel/Desktop/TCM.xlsx")

as.matrix.(Datos)
serieTCM= ts(Datos$Enero, start = c(1,31), frequency = 365)
serieTCM
# grafico de la serie
autoplot(serieTCM, ts.linetype = "dashed")


############################### PARTE 1
############# Suavizacion Exponencial Simple

fcast_serieTCM<- ses(serieTCM, h = 5)
summary(fcast_serieTCM)
plot(fcast_serieTCM, ts.linetype = "dashed", ts.)

############ Suavizacion Exponencial Holt
fcast_TCM <- holt(serieTCM)
summary(fcast_TCM)
plot(fcast_TCM, ts.colour = "blue", ts.linetype = "dashed")

############ Suavizacion Exponencial HW
fcastHW_TCM <- hw(serieTCM, h=2*frequency(Datos$Enero), seasonal = c("additive", "multiplicative"))
summary(fcastHW_TCM)
autoplot(fcastHW_TCM, ts.colour = "blue", ts.linetype = "dashed")

########### serie de tiempo con ggplot2

# autocorrelacion de la serie
autoplot(acf(diff(ts(Datos$Marzo, start = c(1,31), frequency = 365)), plot = FALSE))


