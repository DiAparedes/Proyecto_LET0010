library(ggplot2)
library(rio)
require(tidyverse)
library(olsrr)
library(broom)


full_model = lm(popularity ~ ., datos_a_estudiar1_numericos)

info_full_mod = broom::augment(full_model)

info_full_mod %>% 
  ggplot(aes(.fitted, .resid)) +
  geom_point() +
  labs(x="Valores ajustados", y = "Residuos")

info_full_mod %>% 
  ggplot(aes(sample = .std.resid)) + 
  stat_qq() + # por defecto indica dist. normal
  stat_qq_line() +
  labs(x = "Cuantiles dist. normal teóricos", y = "Cuantiles muestrales") 

shapiro.test(info_full_mod$.std.resid)


datos_a_estudiar1_numericos2 = datos_a_estudiar1_numericos %>% mutate(popularity = popularity^(1/2))

full_model = lm(popularity ~ ., datos_a_estudiar1_numericos2) 

info_full_mod = broom::augment(full_model)

info_full_mod %>% 
  ggplot(aes(.fitted, .resid)) +
  geom_point() +
  labs(x="Valores ajustados", y = "Residuos")

info_full_mod %>% 
  ggplot(aes(sample = .std.resid)) + 
  stat_qq() + # por defecto indica dist. normal
  stat_qq_line() +
  labs(x = "Cuantiles dist. normal teóricos", y = "Cuantiles muestrales") 

shapiro.test(info_full_mod$.std.resid)



####

Press = function(modelo){
  e = resid(modelo)
  n = length(e)
  h = ls.diag(modelo)$hat
  return(press = sum((e/(1 - h))^2) / n)
}

info = function(modelo, full_model){
  info_mod = augment(modelo)
  fila = cbind(length(modelo$coefficients),
               sum(info_mod$.resid^2),
               summary(modelo)$adj.r.squared,
               ols_mallows_cp(modelo, full_model),
               Press(modelo),
               AIC(modelo))
  return(fila)
}


null_model = lm(popularity ~ 1, datos_a_estudiar1_numericos)
full_model = lm(popularity ~ ., datos_a_estudiar1_numericos)


m0 = info(null_model, full_model)
m11 = info(lm(popularity ~ valence, datos_a_estudiar1_numericos), full_model) 
m12 = info(lm(popularity ~ year, datos_a_estudiar1_numericos), full_model)
m13 = info(lm(popularity ~ acousticness, datos_a_estudiar1_numericos), full_model) 
m14 = info(lm(popularity ~ danceability, datos_a_estudiar1_numericos), full_model)
m15 = info(lm(popularity ~ duration_ms, datos_a_estudiar1_numericos), full_model)
m16 = info(lm(popularity ~ energy, datos_a_estudiar1_numericos), full_model)
m17 = info(lm(popularity ~ explicit, datos_a_estudiar1_numericos), full_model)
m18 = info(lm(popularity ~ instrumentalness, datos_a_estudiar1_numericos), full_model)
m19 = info(lm(popularity ~ key, datos_a_estudiar1_numericos), full_model)
m110 = info(lm(popularity ~ liveness, datos_a_estudiar1_numericos), full_model)
m111 = info(lm(popularity ~ loudness, datos_a_estudiar1_numericos), full_model)
m112 = info(lm(popularity ~ mode, datos_a_estudiar1_numericos), full_model)
m113 = info(lm(popularity ~ speechiness, datos_a_estudiar1_numericos), full_model)
m114 = info(lm(popularity ~ tempo, datos_a_estudiar1_numericos), full_model)
m115 = info(lm(popularity ~ numero_de_artistas, datos_a_estudiar1_numericos), full_model)


tabla = rbind(m0, m11, m12, m13, m14, m15, m16, m17, m18, m19, m110, m111, m112,m113, m114, m115 ) 
colnames(tabla) = c("p", "SCE", "R2", "Cp", "Press", "AIC")
rownames(tabla) = c("Nulo", "valence", "year", "acousticness", "danceability", "duration_ms", "energy", "explicit", "instrumentaliness", "key", "liveness", "loudness", "mode", "speechiness", "tempo", "numero_de_artistas")
tabla


m21 = info(lm(popularity ~ year + valence, datos_a_estudiar1_numericos), full_model) 
m22 = info(lm(popularity ~ year + acousticness, datos_a_estudiar1_numericos), full_model) 
m23 = info(lm(popularity ~ year + danceability, datos_a_estudiar1_numericos), full_model)
m24 = info(lm(popularity ~ year + duration_ms, datos_a_estudiar1_numericos), full_model)
m25 = info(lm(popularity ~ year + energy, datos_a_estudiar1_numericos), full_model)
m26 = info(lm(popularity ~ year + explicit, datos_a_estudiar1_numericos), full_model)
m27 = info(lm(popularity ~ year + instrumentalness, datos_a_estudiar1_numericos), full_model)
m28 = info(lm(popularity ~ year + key, datos_a_estudiar1_numericos), full_model)
m29 = info(lm(popularity ~ year + liveness, datos_a_estudiar1_numericos), full_model)
m210 = info(lm(popularity ~ year + loudness, datos_a_estudiar1_numericos), full_model)
m211 = info(lm(popularity ~ year + mode, datos_a_estudiar1_numericos), full_model)
m212 = info(lm(popularity ~ year + speechiness, datos_a_estudiar1_numericos), full_model)
m213 = info(lm(popularity ~ year + tempo, datos_a_estudiar1_numericos), full_model)
m214 = info(lm(popularity ~ year + numero_de_artistas, datos_a_estudiar1_numericos), full_model)

tabla = rbind(m12, m21, m22, m23, m24, m25, m26, m27, m28, m29, m210, m211,m212, m213, m214 ) 

colnames(tabla) = c("p", "SCE", "R2", "Cp", "Press", "AIC")
rownames(tabla) = c("year", "year + valence", "year + acousticness", "year + danceability", "dyear + uration_ms", "year + energy", "year + explicit", "year + instrumentaliness", "year + key", "year + liveness", "year + loudness", "year + mode", "year + speechiness", "year + tempo", "year + numero_de_artistas")
tabla


m31 = info(lm(popularity ~ year + danceability + valence, datos_a_estudiar1_numericos), full_model) 
m32 = info(lm(popularity ~ year + danceability + acousticness, datos_a_estudiar1_numericos), full_model) 
m33 = info(lm(popularity ~ year + danceability + duration_ms, datos_a_estudiar1_numericos), full_model)
m34 = info(lm(popularity ~ year + danceability + energy, datos_a_estudiar1_numericos), full_model)
m35 = info(lm(popularity ~ year + danceability + explicit, datos_a_estudiar1_numericos), full_model)
m36 = info(lm(popularity ~ year + danceability + instrumentalness, datos_a_estudiar1_numericos), full_model)
m37 = info(lm(popularity ~ year + danceability + key, datos_a_estudiar1_numericos), full_model)
m38 = info(lm(popularity ~ year + danceability + liveness, datos_a_estudiar1_numericos), full_model)
m39 = info(lm(popularity ~ year + danceability + loudness, datos_a_estudiar1_numericos), full_model)
m310 = info(lm(popularity ~ year + danceability + mode, datos_a_estudiar1_numericos), full_model)
m311 = info(lm(popularity ~ year + danceability + speechiness, datos_a_estudiar1_numericos), full_model)
m312 = info(lm(popularity ~ year + danceability + tempo, datos_a_estudiar1_numericos), full_model)
m313 = info(lm(popularity ~ year + danceability + numero_de_artistas, datos_a_estudiar1_numericos), full_model)

tabla = rbind(m23, m31, m32, m33, m34, m35, m36, m37, m38, m39, m310, m311,m312, m313 ) 

colnames(tabla) = c("p", "SCE", "R2", "Cp", "Press", "AIC")
rownames(tabla) = c("year+danceability", "year + danceability + valence", "year + danceability + acousticness", "dyear + danceability + uration_ms", "year + danceability + energy", "year + danceability + explicit", "year + danceability + instrumentaliness", "year + danceability + key", "year + danceability + liveness", "year + danceability + loudness", "year + danceability + mode", "year + danceability + speechiness", "year + danceability + tempo", "year + danceability + numero_de_artistas")
tabla


m41 = info(lm(popularity ~ year + danceability + instrumentalness + valence, datos_a_estudiar1_numericos), full_model) 
m42 = info(lm(popularity ~ year + danceability + instrumentalness + acousticness, datos_a_estudiar1_numericos), full_model) 
m43 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms, datos_a_estudiar1_numericos), full_model)
m44 = info(lm(popularity ~ year + danceability + instrumentalness + energy, datos_a_estudiar1_numericos), full_model)
m45 = info(lm(popularity ~ year + danceability + instrumentalness + explicit, datos_a_estudiar1_numericos), full_model)
m46 = info(lm(popularity ~ year + danceability + instrumentalness + key, datos_a_estudiar1_numericos), full_model)
m47 = info(lm(popularity ~ year + danceability + instrumentalness + liveness, datos_a_estudiar1_numericos), full_model)
m48 = info(lm(popularity ~ year + danceability + instrumentalness + loudness, datos_a_estudiar1_numericos), full_model)
m49 = info(lm(popularity ~ year + danceability + instrumentalness + mode, datos_a_estudiar1_numericos), full_model)
m410 = info(lm(popularity ~ year + danceability + instrumentalness + speechiness, datos_a_estudiar1_numericos), full_model)
m411 = info(lm(popularity ~ year + danceability + instrumentalness + tempo, datos_a_estudiar1_numericos), full_model)
m412 = info(lm(popularity ~ year + danceability + instrumentalness + numero_de_artistas, datos_a_estudiar1_numericos), full_model)

tabla = rbind(m36, m41, m42, m43, m44, m45, m46, m47, m48, m49, m410, m411, m412) 

colnames(tabla) = c("p", "SCE", "R2", "Cp", "Press", "AIC")
rownames(tabla) = c("year+danceability+instrumentalness", "year + danceability + instrumentalness + valence", "year + danceability + instrumentalness + acousticness", "dyear + danceability + instrumentalness + duration_ms", "year + danceability + instrumentalness + energy", "year + danceability + instrumentalness + explicit", "year + danceability + instrumentalness + key", "year + danceability + instrumentalness + liveness", "year + danceability + instrumentalness + loudness", "year + danceability + instrumentalness + mode", "year + danceability + instrumentalness + speechiness", "year + danceability + instrumentalness + tempo", "year + danceability + instrumentalness + numero_de_artistas")
tabla





m51 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms + valence, datos_a_estudiar1_numericos), full_model) 
m52 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms + acousticness, datos_a_estudiar1_numericos), full_model) 
m53 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms + energy, datos_a_estudiar1_numericos), full_model)
m54 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms + explicit, datos_a_estudiar1_numericos), full_model)
m55 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms + key, datos_a_estudiar1_numericos), full_model)
m56 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms + liveness, datos_a_estudiar1_numericos), full_model)
m57 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms + loudness, datos_a_estudiar1_numericos), full_model)
m58 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms + mode, datos_a_estudiar1_numericos), full_model)
m59 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms + speechiness, datos_a_estudiar1_numericos), full_model)
m510 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms + tempo, datos_a_estudiar1_numericos), full_model)
m511 = info(lm(popularity ~ year + danceability + instrumentalness + duration_ms + numero_de_artistas, datos_a_estudiar1_numericos), full_model)

tabla = rbind(m43, m51, m52, m53, m54, m55, m56, m57, m58, m59, m510, m511) 

colnames(tabla) = c("p", "SCE", "R2", "Cp", "Press", "AIC")
rownames(tabla) = c("year+danceability+instrumentalness+duration_ms", "year + danceability + instrumentalness + duration_ms + valence", "year + danceability + instrumentalness + duration_ms + acousticness", "year + danceability + instrumentalness + duration_ms + energy", "year + danceability + instrumentalness + duration_ms + explicit", "year + danceability + instrumentalness + duration_ms + key", "year + danceability + instrumentalness + duration_ms + liveness", "year + danceability + instrumentalness + duration_ms + loudness", "year + danceability + instrumentalness + duration_ms + mode", "year + danceability + instrumentalness + duration_ms + speechiness", "year + danceability + instrumentalness + duration_ms + tempo", "year + danceability + instrumentalness + duration_ms + numero_de_artistas")
tabla

### el modelo es tan ... que no es eficiente,
















