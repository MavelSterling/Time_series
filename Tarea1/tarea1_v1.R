##### SERIES DE TIEMPO ########
########### TAREA 1 ###########
###############################
install.packages("readxl")
install.packages("forecast")
install.packages("ggplot2")
###############################
library(readxl)
library(forecast)
library(ggfortify)
###############################
Datos=read_xlsx("E:/BaseSeries.xlsx")
seriePIB= ts(Datos$PIB, start = c(2005,1), frequency = 4)
plot(seriePIB, xlab="Años", ylab="PIB")

########### PARTE 1 ###############
###Suavizacion Exponencial Simple##

fcast_seriePIB<- ses(seriePIB, h = 10)
summary(fcast_seriePIB)
x11()
plot(fcast_seriePIB, main=" ", xlab="Años", ylab="PIB Total")

### Suavizacion Exponencial Holt ## 
fcast <- holt(seriePIB)
summary(fcast)
x11()
plot(fcast, main=" ", xlab="Años", ylab="PIB Total")

## Suavizacion Exponencial HW ##
HoltWin <- hw(seriePIB)
summary(HoltWin)
x11()
plot(HoltWin,main=" ", xlab="Años", ylab="PIB Total")


########### PARTE 2 ###############
#ACF
GrafACF <- acf(seriePIB, main="")
x11()
plot(GrafACF, main=" ")

#DIFF
Estaci<-diff(seriePIB)
DifAc<-acf(Estaci)
x11()
plot(DifAc, main=" ")