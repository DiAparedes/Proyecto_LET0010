###
  library(readr)

datos=read_csv("datos/data.csv")
datos_por_año=read_csv("datos/data_by_year.csv")
datos_por_genero=read_csv("datos/data_by_genres.csv")
datos_a_estudiar1=datos[datos$popularity>75, ]
datos_a_estudiar1$duration_s=datos_a_estudiar1$duration_ms/1000
datos_numericos=datos[,c(-4,-9,-15,-17)]
datos_a_estudiar1$feat=ifelse(datos_a_estudiar1$numero_de_artistas>1,1,0)
 
datos_a_estudiar1=read_csv("datos/datos_a_estudiar1.csv")

muestra_datos=datos[sample(nrow(datos),1736),]

muestra_datos$duration_s=muestra_datos$duration_ms/1000

write_csv(datos_a_estudiar1,"datos/datos_a_estudiar1.csv")

cantft=data.frame(name=c("Solo", "Feat"),value=c(1736-sum(datos_a_estudiar1$feat), sum(datos_a_estudiar1$feat)))

cantftM=data.frame(name=c("Solo", "Feat"),value=c(1736-sum(muestra_datos$featm), sum(muestra_datos$featm)))

write_csv(cantft,"datos/feat.csv")

write_csv(cantftM,"datos/featM.csv")

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





#####



artist22=muestra_datos$artists

artist32=str_remove_all(artist22,"(')")

artist42=artist32 %>% str_replace_all("\\[|\\]","")


as.list(strsplit(artist42,",")[])

numero_de_artistas2=c()
artist52=c()

for(i in 1:length(muestra_datos$artists)){
  artist52=c(artist52,as.list(strsplit(artist42,",")[[i]]))
}


for(i in 1:length(muestra_datos$artists)){
  numero_de_artistas2=c(numero_de_artistas2,length(as.list(strsplit(artist42,",")[[i]])))
}
muestra_datos$numero_de_artista_muestra=numero_de_artistas2

as.list(strsplit(artist42,","))

artist_list_m=as.list(strsplit(artist42,","))

muestra_datos$artistslistmuestra=artist_list_m



muestra_datos$featm=ifelse(muestra_datos$numero_de_artista_muestra>1,1,0)
write_csv(muestra_datos,"datos/muestra_datos.csv")
write_csv(cor_var,"datos/cor_var.csv")
library(gt)
library(gtsummary)
corvarn=data.frame(cor(datos_numericos))
write_csv(corvarn,"datos/cor_varn.csv")

corvarn %>% 
  gt()


