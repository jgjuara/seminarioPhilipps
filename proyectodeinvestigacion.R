# Proyecto de Investigación

library(eph)
library(tidyverse)
library(DescTools)

bases <- get_microdata(year = 2017, trimester = c(1,3), type = "individual",
          vars = c("ANO4", "TRIMESTRE", "CODUSU", "ESTADO", "CAT_OCUP","PONDERA","CH04","CH06","NIVEL_ED","PP04B_COD","P21","PONDIIO"))

bases <- bases$microdata[[1]] %>% 
    rbind(bases$microdata[[2]])

bases <- bases %>% 
    organize_labels()

# variables categoricas como factores
bases <-  bases %>% 
    mutate(CH04 = as_factor(CH04),
           CAT_OCUP =as_factor(CAT_OCUP),
           NIVEL_ED = as_factor(NIVEL_ED),
           P21 = as.numeric(P21),
           ESTADO = as_factor(ESTADO))

# Tablas descriptivas

Freq(bases$ESTADO)
## 17 mil observaciones aprox entran en la poblacion deseada
Freq(eph_2018_3$ESTADO)
Freq(eph_2018_3$CAT_OCUP)
xtabs( ~ ESTADO + CAT_OCUP, eph_2018_3)

## usando la expansion de casos de INDEC representa unas 8.7 millones de personas con un CV de 1.5%
eph_2018_3 %>% 
    filter( ESTADO == "Ocupado" & CAT_OCUP == "Obrero o empleado") %>% 
    summarise(sum(PONDERA))

#seleccion de poblacion de estudio ocupados y trabajadores en relacion de dependencia: edad, estado, cat_ocup
eph_2018_3 <- eph_2018_3 %>% 
    filter( ESTADO == "Ocupado" & CAT_OCUP == "Obrero o empleado")

#seleccionando variables de interés
eph_2018_3 <- eph_2018_3 %>% 
    select(ANO4, PONDERA,CH04,CH06,NIVEL_ED,PP04B_COD,P21,PONDIIO)

#agregando variables desagregadas de actividad por CAES
eph_2018_3 <- eph_2018_3 %>% 
    organize_caes()
eph_2018_3 <- eph_2018_3 %>% 
    mutate(caes_seccion_label = as_factor(caes_seccion_label) )
#analisis descriptivo parte 1: descripción de las variables de interes

#frencuencias absolutas y relativas
# categoricas
Freq(eph_2018_3$CH04)
Freq(eph_2018_3$NIVEL_ED)
Freq(eph_2018_3$caes_seccion_label)
wtd.table(eph_2018_3$caes_seccion_label, weights = eph_2018_3$PONDERA) %>% 
    view()

# numerica
summary(eph_2018_3$P21)
Desc(eph_2018_3$P21, plotit = T)
PlotFdist(eph_2018_3$P21)

#tablas cruzadas
xtabs( ~ CH04 + NIVEL_ED, eph_2018_3)
xtabs( ~ CH04 + caes_seccion_label, eph_2018_3)
xtabs( ~ NIVEL_ED + caes_seccion_label, eph_2018_3)
summary

view(tabla1)
str(tabla1)
summary(tabla1)

questionr::tabs()


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
