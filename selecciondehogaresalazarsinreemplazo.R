# Proyecto de Investigación

library(eph)
library(tidyverse)
library(DescTools)
library(haven)
####----BASES----
#carga de bases
eph2017 <- list()
eph2017[[1]] <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_1.RDS")
eph2017[[2]] <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_2.RDS")
eph2017[[3]] <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_3.RDS")
eph2017[[4]] <- readRDS("~/Documentos/rProyects/seminarioPhilipps/eph_2017_4.RDS")
#suma de bases
eph2017 <- bind_rows(eph2017[[1]][[5]],eph2017[[2]][[5]], eph2017[[3]][[5]], eph2017[[4]][[5]])

#creo una variable "idhogar" que une el CODUSU y el NRO de hogar en una sola linea de caracteres
eph2017 <- eph2017 %>% 
    mutate(idhogar = paste0(CODUSU, NRO_HOGAR))


# me quedo solo con ocupados y obreros
eph2017 <- eph2017 %>% 
    filter(ESTADO == 1 & CAT_OCUP == 3)

# armo una lista de hogares por trimestre
tablahogares <- eph2017 %>% 
    select(idhogar, TRIMESTRE, ANO4) %>% 
    distinct_at(.,.vars = c("idhogar", "TRIMESTRE"), .keep_all = T)

# establezco el valor para RNG a fin de reproducir el resultado
set.seed(1990)

# selecciono del 1er trim 6800 hogares al azar sin reemplazo
# 6800 es un numero cercano al máximo de hogares seleccionables en los 4 trimestres
# este valor fue hallado por reiteraciones del proceso de selección
# el maximo concreto varia segun la selección al azar entre 6800 y 6900

t1 <- tablahogares %>% 
    filter(TRIMESTRE == 1) %>% 
    select(idhogar) %>% 
    sample_n(., size =  6800, replace = F)

# creo variable que agrega el valor del trimestre al id de hogar
# esta variable sera usada en el cruce con el conjunto de datos eph
t1 <- t1 %>% 
    mutate(idhogarTrim = paste0(idhogar, 1))

# selecciono al azar 6800 hogares sin reemplazo
# hogares que aparezcan en el 2do trimetre pero que no hayan sido seleccionados antes
t2 <- tablahogares %>% 
    filter(TRIMESTRE == 2 & idhogar %in% t1$idhogar == F) %>% 
    select(idhogar) %>% 
    sample_n(.,size = 6800, replace = F)


# creo variable que agrega el valor del trimestre al id de hogar
# esta variable sera usada en el cruce con el conjunto de datos ep
t2 <- t2 %>% 
    mutate(idhogarTrim = paste0(idhogar, 2))

# idem t2
# hogares que aparezcan en el 3ero pero que no hayan sido seleccionados en el 1° o 2°
t3 <- tablahogares %>% 
    filter(TRIMESTRE == 3 & idhogar %in% t1$idhogar == F
           & idhogar %in% t2$idhogar == F) %>% 
    select(idhogar) %>%
    sample_n(., size = 6800, replace = F)


# creo variable que agrega el valor del trimestre al id de hogar
# esta variable sera usada en el cruce con el conjunto de datos ep
t3 <- t3 %>% 
    mutate(idhogarTrim = paste0(idhogar, 3))


# idem t2
# hogares que aparezcan en el 4ero pero que no hayan sido seleccionados en el 1°, 2° o 3°
t4 <- tablahogares %>% 
    filter(TRIMESTRE == 4 & idhogar %in% t1$idhogar == F
           & idhogar %in% t2$idhogar == F
           & idhogar %in% t3$idhogar == F) %>% 
    select(idhogar) %>% 
    sample_n(., size =  6800, replace = F)


# creo variable que agrega el valor del trimestre al id de hogar
# esta variable sera usada en el cruce con el conjunto de datos ep
t4 <- t4 %>% 
    mutate(idhogarTrim = paste0(idhogar, 4))

# reuno la seleccion de hogares en una sola tabla
seleccionhogares <- rbind(t1,t2,t3,t4)

# creo una variable que une id de hogar a n° de trimestre
eph2017 <- eph2017 %>% 
    mutate(idhogarTrim = paste0(idhogar, TRIMESTRE))

# selecciono del conjunto de datos de la eph sólo a aquellos individuos que
# pertenezcan a los hogares seleccionados
eph2017 <- semi_join(eph2017, seleccionhogares, by = "idhogarTrim")

write_rds(eph2017, "eph_2017_seleccioncasos.RDS", compress = "gz")
write_sav(eph2017, path = "eph_2017_seleccioncasos.sav", compress = T)
