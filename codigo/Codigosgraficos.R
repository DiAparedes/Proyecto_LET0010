
library(readr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(gt)
library(gtsummary)


datos=read_csv("datos/data.csv")
datos_por_año=read_csv("datos/data_by_year.csv")
datos_por_genero=read_csv("datos/data_by_genres.csv")


datos_numericos=datos[,c(-4,-9,-15,-17)]


a=c()


for(i in 1:15){

a=c(a,cor(datos_numericos$popularity,datos_numericos[i]))

}  

a


  

datos$Popularidad=ifelse(datos$popularity >75, "Popular","No Popular")
grafico1=ggplot(data = datos)+
  geom_histogram(aes(x=datos$popularity, fill=Popularidad),bins = 30)+
  labs(title="Número de canciones según su popularidad",
       x="Popularidad", y="Número de Canciones")


datosye=data.frame(datos$year,datos$explicit)
summary(datosye)


datosye=tapply(datos$explicit,datos$year,FUN = sum)

datosye2=data.frame(seq(1921,2020),datosye)

grafico2=datosye2 %>% 
  ggplot(aes(datosye2$seq.1921..2020.,datosye2$datosye))+
  geom_line(size=2,col="darkgreen" )+
  labs(title="Canciones con letras explicitas por año",
       x="Año",
       y="Cantidad de canciones")


class(datos$artists)

