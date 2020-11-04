# Proyecto de Investigación

library(eph)
library(tidyverse)
library(data.table)

eph_2018_3 <- readRDS("~/Documentos/rProyects/EPHfiles/eph_2018_3.RDS")

eph_2018_3 <- eph_2018_3$microdata[[1]]

eph_2018_3 <- eph_2018_3 %>% 
    organize_caes()

eph_2018_3 <- eph_2018_3 %>% 
    organize_labels()

eph_2018_3 <- eph_2018_3 %>% 
    mutate(CH04 = as_factor(CH04),
           NIVEL_ED = as_factor(NIVEL_ED))

# subtabla x region, estado, categoria de ocupacion

dfeph_2018_3_gba <- eph_2018_3 %>% 
    filter(REGION == 01, ESTADO == 1 , CAT_OCUP == 3)

#seleccion variables de interés

dfeph_2018_3_gba <- dfeph_2018_3_gba %>% 
    select(ANO4, PONDERA,CH04,CH06,NIVEL_ED,PP04B_COD,P21,PONDIIO)


dfeph_2018_3_gba <- dfeph_2018_3_gba %>% 
    organize_caes()

totalPONDERA <- sum(dfeph_2018_3_gba$PONDERA)
totalPONDIIO <- sum(dfeph_2018_3_gba$PONDIIO)

tabla1 <- dfeph_2018_3_gba %>% 
    mutate(CH04 = as_factor(CH04), NIVEL_ED = as_factor(NIVEL_ED)) %>% 
    group_by(CH04, NIVEL_ED) %>% 
    summarise( n = sum(PONDERA))

tabla1 <- dfeph_2018_3_gba %>% 
    mutate(CH04 = as_factor(CH04), NIVEL_ED = as_factor(NIVEL_ED)) %>% 
    group_by(CH04, NIVEL_ED) %>% 
    summarise(n = n())

tabla2 <- dfeph_2018_3_gba %>% 
    group_by(caes_seccion_label) %>% 
    summarise(n = n())
##
linealed <- lm(P21 ~ NIVEL_ED, dfeph_2018_3_gba)
summary(linealed)
#

##
linealseccion <- lm(P21 ~ caes_seccion_label, dfeph_2018_3_gba)
summary(linealseccion)
# muchas categorias no resultan significativas, es problematico trabajar a este nivel de desagregacion en GBA

##
linealseccionTotal <- lm(P21 ~ caes_seccion_label, eph_2018_3)
summary(linealseccionTotal)
# buena significatividad en muchas categorías, *atencion* transporte deja de ser significativo.

linealedTotal <- lm(P21 ~ NIVEL_ED, eph_2018_3)
summary(linealedTotal)

linealseccion_edTotal <- lm(P21 ~ caes_seccion_label + NIVEL_ED, eph_2018_3)
summary(linealseccion_edTotal)

linealseccionXedTotal <- lm(P21 ~ caes_seccion_label * NIVEL_ED, eph_2018_3)
summary(linealseccionXedTotal)
