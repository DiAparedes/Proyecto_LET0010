library(gridExtra)
library(readr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(gt)
library(gtsummary)


datos=read_csv("datos/data.csv")
datos_por_año=read_csv("datos/data_by_year.csv")
datos_por_genero=read_csv("datos/data_by_genres.csv")
datos_a_estudiar1=datos[datos$popularity>75, ]
datos_a_estudiar1$duration_s=datos_a_estudiar1$duration_ms/1000
datos_numericos=datos[,c(-4,-9,-15,-17)]
datos_a_estudiar1$feat=ifelse(datos_a_estudiar1$numero_de_artistas>1,1,0)

a=c()


for(i in 1:15){

a=c(a,cor(datos_numericos$popularity,datos_numericos[i]))

}  

a


#codigo grafico1

datos$Popularidad=ifelse(datos$popularity >76, "Popular","No Popular")
grafico1=ggplot(data = datos)+
  geom_histogram(aes(x=datos$popularity, fill=Popularidad),bins = 30)+
  labs(title="Número de canciones según su popularidad",
       x="Popularidad", y="Número de Canciones")+
  scale_fill_manual(values = c("#b3b3b3", "#1db954"))+
  theme_bw()+
  geom_vline(xintercept = 76,linetype="dashed",color="black")+
  annotate("text", x = 87, y = 25000, label = "Canciones a estudiar")

#codigo grafico 2

datosye=data.frame(datos$year,datos$explicit)


datosye=tapply(datos$explicit,datos$year,FUN = sum)

datosye2=data.frame(seq(1921,2020),datosye)

grafico2=datosye2 %>% 
  ggplot(aes(datosye2$seq.1921..2020.,datosye2$datosye))+
  geom_line(size=2,col="#1db954" )+
  theme_bw()+
  labs(title="Canciones con letras explicitas por año",
       x="Año",
       y="Cantidad de canciones")

##grafico bailabilidad por año,

grafby=datos_por_año %>% 
  ggplot(aes(year,danceability))+
  geom_line(size=2,col="#1db954")+
  theme_bw()+
  labs(
       x="Año",
       y="Bailabilidad")
##grafico acusticidad por año,
grafacy=datos_por_año %>% 
  ggplot(aes(year,acousticness))+
  geom_line(size=2,col="#1db954")+
  theme_bw()+
  geom_vline(xintercept = 1951.5,linetype="dashed",color="black")+
  labs(
    x="Año",
    y="Acusticidad(Naturalidad del sonido)")+
  annotate("text", x = 1970, y = 0.95, label = "Creación primer sintetizador de música")

## graf loud- inst, energy

grafinty=datos_por_año %>% 
  ggplot(aes(year,instrumentalness))+
  geom_line(size=2,col="#1db954")+
  theme_bw()+
  labs(
    x="Año",
    y="Instrumentalidad")



grafeny=datos_por_año %>% 
  ggplot(aes(year,energy))+
  geom_line(size=2,col="#1db954")+
  theme_bw()+
  labs(
    x="Año",
    y="Energía")


grafloy=datos_por_año %>% 
  ggplot(aes(year,loudness))+
  geom_line(size=2,col="#1db954")+
  theme_bw()+
  labs(
    x="Año",
    y="Volumen")

#codigo grafico 3, a arreglar en un futuro


muestra_datos=datos[sample(nrow(datos),1736),]

muestra_datos$duration_s=muestra_datos$duration_ms/1000

#####
grafico3=ggplot(datos_a_estudiar1,aes(x=datos_a_estudiar1$duration_s,y=datos_a_estudiar1$popularity))+
  geom_point()

median(datos_a_estudiar1$duration_s)
####
#graficos de variables impprtantes

####
#### durs 205.09
####

labs(title="Duranción en segundos de canciones populares")
     
labs(title="Duranción en segundos de canciones no populares")

grafico4=ggplot(data=datos_a_estudiar1)+
  geom_histogram(aes(x=duration_s),col= "black",fill="#1db954")+
  theme_bw()+
  labs(x="Duración",
       y="Cantidad de canciones")

  
grafico42=ggplot(data=muestra_datos)+
  geom_histogram(aes(x=duration_s),col= "black",fill="#b3b3b3")+
  theme_bw()+
  labs(x="Duración",
       y="Cantidad de canciones")
  

grid.arrange(grafico4,grafico42,ncol=2)
  
####
####dancs 0.731
####

grafico5=ggplot(data=datos_a_estudiar1)+
  geom_histogram(aes(x=danceability),col= "black",fill="#1db954")+
  theme_bw()+
  labs(title="Bailabilidad de canciones populares",
       x="Bailabilidad",
       y="Cantidad de canciones")



grafico52=ggplot(data=muestra_datos)+
  geom_histogram(aes(x=danceability),col= "black",fill="#b3b3b3")+
  theme_bw()+
  labs(title="Bailabilidad de canciones no populares",
       x="Bailabilidad",
       y="Cantidad de canciones")

grid.arrange(grafico5,grafico52,ncol=2)


####
####ints 5.22e-05
####

grafico6=ggplot(data=datos_a_estudiar1)+
  geom_histogram(aes(x=instrumentalness),col= "black",fill="#1db954")+
  theme_bw()+
  labs(title="Instrumentalidad de canciones populares",
       x="Instrumentalidad",
       y="Cantidad de canciones")



grafico62=ggplot(data=muestra_datos)+
  geom_histogram(aes(x=instrumentalness),col= "black",fill="#b3b3b3")+
  theme_bw()+
  labs(title="Instrumentalidad de canciones no populares",
       x="Instrumentalidad",
       y="Cantidad de canciones")

grid.arrange(grafico6,grafico62,ncol=2)

####
####energy 0.573
####



grafico7=ggplot(data=datos_a_estudiar1)+
  geom_histogram(aes(x=energy),col= "black",fill="#1db954")+
  theme_bw()+
  labs(title="Energia de canciones populares",
       x="Energia",
       y="Cantidad de canciones")





grafico72=ggplot(data=muestra_datos)+
  geom_histogram(aes(x=energy),col= "black",fill="#b3b3b3")+
  theme_bw()+  
  labs(title="Energia de canciones no populares",
                    x="Energia",
                    y="Cantidad de canciones")


grid.arrange(grafico7,grafico72,ncol=2)

####
####loud=-10.059
####


grafico8=ggplot(data=datos_a_estudiar1)+
  geom_histogram(aes(x=loudness),col= "black",fill="#1db954")+
  theme_bw()+
  labs(title="Volumen de canciones populares",
       x="volumen",
       y="Cantidad de canciones")

grafico82=ggplot(data=muestra_datos)+
  geom_histogram(aes(x=loudness),col= "black",fill="#b3b3b3")+
  theme_bw()+
  labs(title="Volumen de canciones no populares",
       x="Volumen",
       y="Cantidad de canciones")

grid.arrange(grafico8,grafico82,ncol=2)

####
####nart  2
####

grafico9=ggplot(data=datos_a_estudiar1)+
  geom_histogram(aes(x=numero_de_artistas),col= "black",fill="#1db954")+
  theme_bw()+
  labs(
       x="Numero de artistas",
       y="Cantidad de canciones")+



grafico92=ggplot(data=muestra_datos)+
  geom_histogram(aes(x=numero_de_artista_muestra),col= "black",fill="#b3b3b3")+
  theme_bw()+
  labs(
       x="Numero de artistas",
       y="Cantidad de canciones")

grid.arrange(grafico9,grafico92,ncol=2)

####
####
####

cantft=data.frame(name=c("Solo", "Feat"),value=c(1736-sum(datos_a_estudiar1$feat), sum(datos_a_estudiar1$feat)))

cantftM=data.frame(name=c("Solo", "Feat"),value=c(1736-sum(muestra_datos$featm), sum(muestra_datos$featm)))


grafico10=ggplot(data=cantft,aes(x=name,y=value))+
  geom_bar(stat="identity" ,col= "black",fill="#1db954",width=0.5)+
  theme_bw()+
  labs(x="Tipo de canción",
       y="Cantidad de canciones")




grafico102=ggplot(data=cantftM,aes(x=name,y=value))+
  geom_bar(stat="identity" ,col= "black",fill="#b3b3b3",width=0.5)+
  theme_bw()+
  labs(x="Tipo de canción",
       y="Cantidad de canciones")

###


hist(datos_a_estudiar1$instrumentalness)
####
library(stringr)



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


which(datos_por_artista$artists=="Bad Bunny")

plot(datos_a_estudiar1_numericos$popularity,datos_a_estudiar1)

class(plot(datos_a_estudiar1_numericos))


aa=data.frame(datos_por_artista[2026,])


artista_principal=c()

for(i in 1:1736){
  artista_principal=c(artista_principal,artist_list[[i]][1])
}

artista_principal


datos_a_estudiar1$mainartist=artista_principal


mas11=datos_a_estudiar1 %>% 
  slice_max(popularity,n=12)


mas12=unique(mas11[23])
mas13=sapply(mas12,head,n=10)

numero_de_canciones=c()

for(i in 1:10){
numero_de_canciones=c(numero_de_canciones,length(which(datos_a_estudiar1$mainartist==mas13[i,])))
}




canyncan=data.frame(mas13,numero_de_canciones)
canyncan %>% 
  gt() %>% 
  summary_rows(
    columns = numero_de_canciones,
    fns = list(total="sum")
  )









