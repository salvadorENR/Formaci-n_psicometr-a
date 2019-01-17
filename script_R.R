
#Cargar la base de datos
BBDD_Mat=read.csv("BBDD_Mat.csv")


#*********************************I. Análisis inicial***********************************************
b1=as.data.frame(BBDD_Mat)
attach(b1)
#crear función
Frecuencias <- function(il) {
  N_observaciones=length(b1$ID)
  t1=0
  t0=0
  t9=0
  i=0
  for (i in 1:N_observaciones) {
    if (il[i]==1) {
      t1=t1+1
    } else {
      if (il[i]==0){
        t0=t0+1
      }
      else{
        print(il)
        t9=t9+1 
      }
    }
  }
  vector1=c(t1,t0,t9)
  return(vector1)
}

f1= Frecuencias(i1)
f2= Frecuencias(i2)
f3= Frecuencias(i3)
f4= Frecuencias(i4)
f5= Frecuencias(i5)
f6= Frecuencias(i6)
f7= Frecuencias(i7)
f8= Frecuencias(i8)
f9= Frecuencias(i9)
f10= Frecuencias(i10)
f11= Frecuencias(i11)
f12= Frecuencias(i12)
f13= Frecuencias(i13)
f14= Frecuencias(i14)
f15= Frecuencias(i15)

distribución_respuestas=rbind.data.frame(f1,f2,f3,f4,f5,f6,f7,f8,f9,
                                         f10,f11,f12,f13,f14,f15)
colnames(distribución_respuestas) <- c("Correcto","Incorrecto","No_responde") 
rownames(distribución_respuestas) <- c("ítem 1","ítem 2","ítem 3","ítem 4","ítem 5","ítem 6"
                                       ,"ítem 7","ítem 8","ítem 9","ítem 10","ítem 11","ítem 12"
                                       ,"ítem 13","ítem 14","ítem 15")

distribución_respuestas #ver la tabla de distribución de frecuencias por ítems
hist(distribución_respuestas$Correcto)
hist(distribución_respuestas$Incorrecto)
hist(distribución_respuestas$No_responde)
sort(distribución_respuestas$Correcto)
barplot(distribución_respuestas$Correcto,names.arg = c("ítem 1","ítem 2", "ítem 3","ítem 4","ítem 5","ítem 6"
                                                       ,"ítem 7","ítem 8","ítem 9","ítem 10","ítem 11","ítem 12"
                                                       ,"ítem 13","ítem 14","ítem 15"))
barplot(distribución_respuestas$Incorrecto)
barplot(distribución_respuestas$No_responde)


barplot(distribución_respuestas$Correcto)


#**********************************II.	Análisis Global del Test**********************************
#Estadística descriptiva
sapply(TOTAL,mean, na.rm=TRUE)
summary(TOTAL)

#library(Hmisc)
#describe(TOTAL) 

#library(pastecs)
#stat.desc(TOTAL)

library(psych) #Está función es la que ofrece los resultados más completos 
describe(TOTAL)

# Histograma de la distribución de resultados y la curva normal

hist(TOTAL, freq=FALSE,col= "Black", border = "White",main = "Distribución de los resultados",xlab = "Resultados",ylab = "Densidad")
curve(dnorm(x, mean=mean(TOTAL), sd=sd(TOTAL)), add=TRUE, col="red")



#****************************** III.	Análisis Individual de Ítems *******************************

#+++++++++++++++++++++++++++++++ Grado de dificultad ++++++++++++++++++++++++++++++++++++++++
library(psychometric)
item.exam(b1, y = NULL, discrim = FALSE)

item=c("i1", "i2","i3","i4","i5","i6","i7","i8","i9","i10","i11","i12","i13","i14","i15")

gd= c(0.9046794,0.7710861,0.5167533,0.7420566,0.2807626,0.3382438,0.6390815,0.5684575,0.8019931,0.5092432,0.4692374,0.6707106,0.5079434,0.1067302,0.3703062)
tabla_GD=cbind.data.frame(item,gd)
tabla_GD
attach(tabla_GD)
barplot(gd,names.arg = c("ítem 1","ítem 2", "ítem 3","ítem 4","ítem 5","ítem 6"
                         ,"ítem 7","ítem 8","ítem 9","ítem 10","ítem 11","ítem 12"
                         ,"ítem 13","ítem 14","ítem 15"))

#++++++++++++++++++++++++++++++ Correlación ítem - rest ++++++++++++++++++++++++++++++++++++
item.exam(b1, y = NULL, discrim = FALSE)
item.total(cbind.data.frame(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15))



#*************************************** IV.	Fiabilidad ***************************************

alpha(cbind.data.frame(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15))

