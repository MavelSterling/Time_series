#Punto 2 con una distribuci?n leptocurtica para los errores
z<-0
tau<-0
for(j in 1:10000){
  z[1]<-0
  for(i in 2:50){
    z[i]<-z[i-1]+rnorm(1,8,0.5)}
  
  difZ<-diff(z)
  zt_1<-z[1:49]
  modelo<-summary(lm(difZ~zt_1-1))
  pi<-modelo$coefficients[1]
  stderror<-modelo$coefficients[2]
  tau[j]<-pi/(stderror)
} 
tau
quantile(tau, prob=c(0.01,0.05,0.1))
#####
library(ggplot2)

library(gridExtra)

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

grid.arrange(h1, h2, ncol=2, nrow = 1)
