
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


#codigo grafico1

datos$Popularidad=ifelse(datos$popularity >75, "Popular","No Popular")
grafico1=ggplot(data = datos)+
  geom_histogram(aes(x=datos$popularity, fill=Popularidad),bins = 30)+
  labs(title="Número de canciones según su popularidad",
       x="Popularidad", y="Número de Canciones")+
  geom_vline(xintercept = 74,linetype="dashed",color="black")+
  annotate("text", x = 87, y = 25000, label = "Canciones a estudiar")

#codigo grafico 2
datosye=data.frame(datos$year,datos$explicit)


datosye=tapply(datos$explicit,datos$year,FUN = sum)

datosye2=data.frame(seq(1921,2020),datosye)

grafico2=datosye2 %>% 
  ggplot(aes(datosye2$seq.1921..2020.,datosye2$datosye))+
  geom_line(size=2,col="darkgreen" )+
  labs(title="Canciones con letras explicitas por año",
       x="Año",
       y="Cantidad de canciones")


#codigo grafico 3, a arreglar en un futuro

grafico3=qqplot(datos$year,datos$popularity,main="Popularidad de las canciones cada año",
                xlab="Año",
                ylab="popularidad de la canción")
  

####
library(stringr)

datos_a_estudiar1=datos[datos$popularity>75, ]

artist2=datos_a_estudiar1$artists

artist3=str_remove_all(artist2,"(')")

artist4=artist3 %>% str_replace_all("\\[|\\]","")


as.list(strsplit(artist4,",")[])

numero_de_artistas=c()
artist5=c()

for(i in 1:length(datos_a_estudiar1$artists)){
  artist5=c(artist5,as.list(strsplit(artist4,",")[[i]]))
}


for(i in 1:length(datos_a_estudiar1$artists)){
  numero_de_artistas=c(numero_de_artistas,length(as.list(strsplit(artist4,",")[[i]])))
}
datos_a_estudiar1$numero_de_artistas=numero_de_artistas

as.list(strsplit(artist4,","))

artist_list=as.list(strsplit(artist4,","))

datos_a_estudiar1$artistslist=artist_list
datos

library(gt)
library(gtsummary)

Correlación=c()

for(i in 1:16){
  
  Correlación=c(Correlación,cor(datos_a_estudiar1_numericos$popularity,datos_a_estudiar1_numericos[i]))
  
}  
Correlación

datos_a_estudiar1=data.frame(datos_a_estudiar1,numero_de_artistas)

datos_a_estudiar1_numericos=datos_a_estudiar1[,c(-4,-9,-15,-17,-20,-21)]
cor_var=data.frame(variable=names(datos_a_estudiar1_numericos),Correlación)

names(datos_a_estudiar1)


qqplot(datos_a_estudiar1$acousticness,datos_a_estudiar1$popularity,main="Popularidad de las canciones cada año",
       xlab="Año",
       ylab="popularidad de la canción")

plot(datos_a_estudiar1$valence,datos_a_estudiar1$popularity)


library(dplyr)
library(tidyr)
##tablas

cor_var %>%#al anexo
  gt()



###
which(datos$popularity==100)
#valores cancion mas popular
cancion_mas_popular=datos[19612,]
dakiti=data.frame(cancion_mas_popular)[,-20]


dakiti %>%
  gt()

 


