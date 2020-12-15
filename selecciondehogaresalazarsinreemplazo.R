# Proyecto de Investigación

library(eph)
library(tidyverse)
library(DescTools)

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

# obtengo la lista de hogares y en que trimestre se observaron
hogaresportrimestre <- eph2017 %>% 
    select(idhogar, TRIMESTRE, ANO4) %>% 
    #agrupo primero por idhogar y dps por trimestre
    pivot_wider(names_from = TRIMESTRE, values_from = TRIMESTRE, values_fn = list(TRIMESTRE = is.vector))

# armo una lista
tablahogares <- eph2017 %>% 
    select(idhogar, TRIMESTRE, ANO4) %>% 
    distinct_at(.,.vars = "idhogar", .keep_all = T)

t1 <- tablahogares %>% 
    filter(TRIMESTRE == 1) %>% 
    select(idhogar) %>% 
    sample_n(., size =  4917, replace = F)

t1 <- t1 %>% 
    mutate(idhogarTrim = paste0(idhogar, 1))

t2 <- tablahogares %>% 
    filter(TRIMESTRE == 2 & idhogar %in% t1$idhogar == F) %>% 
    select(idhogar) %>% 
    sample_n(.,size = 4917, replace = F)

t2 <- t2 %>% 
    mutate(idhogarTrim = paste0(idhogar, 2))

t3 <- tablahogares %>% 
    filter(TRIMESTRE == 3 & idhogar %in% t1$idhogar == F
           & idhogar %in% t2$idhogar == F) %>% 
    select(idhogar) %>%
    sample_n(., size = 4917, replace = F)

t3 <- t3 %>% 
    mutate(idhogarTrim = paste0(idhogar, 3))

t4 <- tablahogares %>% 
    filter(TRIMESTRE == 4 & idhogar %in% t1$idhogar == F
           & idhogar %in% t2$idhogar == F
           & idhogar %in% t3$idhogar == F) %>% 
    select(idhogar) %>% 
    sample_n(., size =  4917, replace = F)

t4 <- t4 %>% 
    mutate(idhogarTrim = paste0(idhogar, 4))

seleccionhogares <- rbind(t1,t2,t3,t4)

eph2017 <- eph2017 %>% 
    mutate(idhogarTrim = paste0(idhogar, TRIMESTRE))

eph2017 <- semi_join(eph2017, seleccionhogares, by = "idhogarTrim")

sum(t1$idhogar == t2$idhogar)
sum(t2$idhogar == t3$idhogar)
sum(t1$idhogar == t4$idhogar)

eph2017 %>% 
    group_by(TRIMESTRE) %>% 
    summarise(n(),
              n_distinct(idhogar))
