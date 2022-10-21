# Julieth Natalia Salazar Vargas 
# Mavelyn Sterling Londono


## funcion para el estimador de la distribucion normal
estimador_Gauss<-function(media, sigma, NumSim){
  
  # definicion de variables
  n<- c(30,50,100,500)

  # Matriz de los estimadores
  Rho_N<-matrix(0,NumSim+2,length(n)*5)
  Rho_N[1,]<-c(rep(n[1],5),rep(n[2],5),rep(n[3],5),rep(n[4],5))
  Rho_N[2,1:5]<-c("p1","p2","p3","p4","p5")#n=30
  Rho_N[2,6:10]<-c("p1","p2","p3","p4","p5")#n=50
  Rho_N[2,11:15]<-c("p1","p2","p3","p4","p5")#n=100
  Rho_N[2,16:20]<-c("p1","p2","p3","p4","p5")#n=500
  
  #ruidoBlanco_Rho=0
  #Rho_RB <-0
  
  
  for (i in 1:length(n)) {
    for (j in 1:NumSim) {
      x<- ts(round(rnorm(n[i], media, sigma),4)) # se crea el ruido blanco
      media_RB<- mean(x) # media del ruido blanco segun la muestra
      r_0<-sum((x-media_RB)^2)
      Rho=numeric(5)
      r_k<-0
        for(k in 1:5){
          for (t in 1:(n[i]-k)) {
                r_k<-r_k+((x[t]-media_RB)*(x[t+k]))}
        Rho[k]<-(round(r_k/r_0,3))
      }
      if(n[i]==30){ # Para la muestra 30
        Rho_N[j+2,1:5]<-as.numeric(Rho)
      }
      if(n[i]==50){ # Para la muestra 50
        Rho_N[j+2,6:10]<-as.numeric(Rho)
      }
      if(n[i]==100){ # Para la muestra 100
        Rho_N[j+2,11:15]<-as.numeric(Rho)
      }
      if(n[i]==500){ # Para la muestra 500
        Rho_N[j+2,16:20]<-as.numeric(Rho)
      }
    }
    
  }
  
  return(Rho_N)
}

## funcion para el estimador de la distribucion tstudent
estimador_tstudent<-function(parametro, NumSim){
  
  # definicion de variables
  
  n<- c(30,50,100,500)
  # Matriz de los estimadores
  Rho_N<-matrix(0,NumSim+2,length(n)*5)
  Rho_N[1,]<-c(rep(n[1],5),rep(n[2],5),rep(n[3],5),rep(n[4],5))
  Rho_N[2,1:5]<-c("p1","p2","p3","p4","p5")#n=30
  Rho_N[2,6:10]<-c("p1","p2","p3","p4","p5")#n=50
  Rho_N[2,11:15]<-c("p1","p2","p3","p4","p5")#n=100
  Rho_N[2,16:20]<-c("p1","p2","p3","p4","p5")#n=500
  
  #ruidoBlanco_Rho=0
  #Rho_RB <-0

  for (i in 1:length(n)) {
    for (j in 1:NumSim) {
      x<- ts(round(rt(n[i], parametro),4)) # se crea el ruido blanco
      media_RB<- mean(x) # media del ruido blanco segun la muestra
      r_0<-sum((x-media_RB)^2)
      Rho=numeric(5)
      r_k<-0
      for(k in 1:5){
        
        for (t in 1:(n[i]-k)) {
          
          r_k<-r_k+((x[t]-media_RB)*(x[t+k]))
        }
        
        Rho[k]<-(round(r_k/r_0,3))
        
      }
      if(n[i]==30){ # Para la muestra 30
        Rho_N[j+2,1:5]<-as.numeric(Rho)
      }
      if(n[i]==50){ # Para la muestra 50
        Rho_N[j+2,6:10]<-as.numeric(Rho)
      }
      if(n[i]==100){ # Para la muestra 100
        Rho_N[j+2,11:15]<-as.numeric(Rho)
      }
      if(n[i]==500){ # Para la muestra 500
        Rho_N[j+2,16:20]<-as.numeric(Rho)
      }
    }
    #ruidoBlanco_Rho[i] <- acf(x, type = "correlation", plot = T)
    #Rho_RB[i] = ruidoBlanco_Rho$acf
    
    
    
  }
  
  return(Rho_N)
}

## funcion para el estimador de la distribucion gamma
estimador_Gamma<-function(alpha, beta, NumSim){
  
  # definicion de variables
  n<- c(30,50,100,500)

  # Matriz de los estimadores
  Rho_N<-matrix(0,NumSim+2,length(n)*5)
  Rho_N[1,]<-c(rep(n[1],5),rep(n[2],5),rep(n[3],5),rep(n[4],5))
  Rho_N[2,1:5]<-c("p1","p2","p3","p4","p5")#n=30
  Rho_N[2,6:10]<-c("p1","p2","p3","p4","p5")#n=50
  Rho_N[2,11:15]<-c("p1","p2","p3","p4","p5")#n=100
  Rho_N[2,16:20]<-c("p1","p2","p3","p4","p5")#n=500
  
  #ruidoBlanco_Rho=0
  #Rho_RB <-0
  
  for (i in 1:length(n)) {
    for (j in 1:NumSim) {
      x<- ts(round(rgamma(n[i], alpha, beta),4)) # se crea el ruido blanco
      media_RB<- mean(x) # media del ruido blanco segun la muestra
      r_0<-sum((x-media_RB)^2)
      Rho=numeric(5)
      r_k<-0
      for(k in 1:5){
        
        for (t in 1:(n[i]-k)) {
          
          r_k<-r_k+((x[t]-media_RB)*(x[t+k]))
        }
        
        Rho[k]<-(round(r_k/r_0,3))
        
      }
      if(n[i]==30){ # Para la muestra 30
        Rho_N[j+2,1:5]<-as.numeric(Rho)
      }
      if(n[i]==50){ # Para la muestra 50
        Rho_N[j+2,6:10]<-as.numeric(Rho)
      }
      if(n[i]==100){ # Para la muestra 100
        Rho_N[j+2,11:15]<-as.numeric(Rho)
      }
      if(n[i]==500){ # Para la muestra 500
        Rho_N[j+2,16:20]<-as.numeric(Rho)
      }
    }
    #ruidoBlanco_Rho[i] <- acf(x, type = "correlation", plot = T)
    #Rho_RB[i] = ruidoBlanco_Rho$acf
    
    
    
  }
  
  return(Rho_N)
}


# Simulacion de las funciones para el estimador
sim1<- estimador_Gauss(0,1,1000);sim1  
sim2<- estimador_tstudent(15,1000);sim2
sim3<- estimador_Gamma(2,5,1000);sim3

library(normtest)
## Prueba jarque-Bera para los estimadores
prueba_estimador <- function(estimador){

JB<-matrix(0,5,4)
JB<-as.data.frame(JB)
rownames(JB)<-c("p1","p2","p3","p4","p5")
colnames(JB)<-c("T = 30","T = 50","T = 100","T = 500")

# Para los estimadores de muestra 30
JB[1,1]<-jb.norm.test(as.numeric(estimador[3:1002,1]))$p.value
JB[2,1]<-jb.norm.test(as.numeric(estimador[3:1002,2]))$p.value
JB[3,1]<-jb.norm.test(as.numeric(estimador[3:1002,3]))$p.value
JB[4,1]<-jb.norm.test(as.numeric(estimador[3:1002,4]))$p.value
JB[5,1]<-jb.norm.test(as.numeric(estimador[3:1002,5]))$p.value

# Para los estimadores de muestra 50
JB[1,2]<-jb.norm.test(as.numeric(estimador[3:1002,6]))$p.value
JB[2,2]<-jb.norm.test(as.numeric(estimador[3:1002,7]))$p.value
JB[3,2]<-jb.norm.test(as.numeric(estimador[3:1002,8]))$p.value
JB[4,2]<-jb.norm.test(as.numeric(estimador[3:1002,9]))$p.value
JB[5,2]<-jb.norm.test(as.numeric(estimador[3:1002,10]))$p.value

# Para los estimadores de muestra 100
JB[1,3]<-jb.norm.test(as.numeric(estimador[3:1002,11]))$p.value
JB[2,3]<-jb.norm.test(as.numeric(estimador[3:1002,12]))$p.value
JB[3,3]<-jb.norm.test(as.numeric(estimador[3:1002,13]))$p.value
JB[4,3]<-jb.norm.test(as.numeric(estimador[3:1002,14]))$p.value
JB[5,3]<-jb.norm.test(as.numeric(estimador[3:1002,15]))$p.value

# Para los estimadores de muestra 500
JB[1,4]<-jb.norm.test(as.numeric(sim1[3:1002,16]))$p.value
JB[2,4]<-jb.norm.test(as.numeric(sim1[3:1002,17]))$p.value
JB[3,4]<-jb.norm.test(as.numeric(sim1[3:1002,18]))$p.value
JB[4,4]<-jb.norm.test(as.numeric(sim1[3:1002,19]))$p.value
JB[5,4]<-jb.norm.test(as.numeric(sim1[3:1002,20]))$p.value

return(JB)
}

# prueba para el estimador segun la distribucion
prueba_estimador(sim1)
prueba_estimador(sim2)
prueba_estimador(sim3)
