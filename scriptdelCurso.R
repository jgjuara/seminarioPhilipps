#Seminario Philipp

library(eph)
library(tidyverse)

# cargo la tabla con los errores muestrales de la EPH
error201403TotalAglom <- read_csv(file = "~/Descargas/EPH/error201403TotalAglom.csv")

# construyo una función para facilitar la seleccion del DS o CV de la tabla de errores muestrales
# la funcion selecciona el elemento del vector "resultado" que cumpla la condicion de que
# la distancia entre el valor de referencia y el valor estimado es la mínima para el conjunto de elementos

closest <- function(valor, referencia, resultado = referencia) {
    resultado[which.min(abs(referencia-valor))]
}

#TP 2 CALCULO DE TASAS

#cargo la ultima eph individual disponible
eph202001 <- get_microdata(year = 2020, trimester = 1, type = "individual",
                           vars = c("CH04", "CH06", "ESTADO", "PONDERA"))

#etiqueto las variables
eph202001 <- organize_labels(eph202001)

# calculo de valores para varones y mujeres de 25 a 65 años
grupo25a65 <- eph202001 %>% 
    #filtro por edad y selecciono solo grupo etario deseado
    filter(CH06 >= 25 & CH06<= 65) %>% 
    #divido por genero para calcular por separado cada grupo
    group_by(genero = as_factor(CH04)) %>% 
    #summarise envuelve los calculos deseados
    summarise(edad = "25 a 65", #creo una columna edad para tener a mano la referencia,
              PEA = sum(PONDERA[ESTADO == 1 | ESTADO == 2]), #calculo PEA bruto como suma de la ponderacion de casos que
              # cumplan ESTADO == ocupado ó ESTADO == desocupado
              totalPoblacion = sum(PONDERA), #procedimiento analogo para el total
              tasaActividad = round(PEA/totalPoblacion*100,2), #calculo la tasa y redondeo a 2 digitos
              CVppPEA = closest(PEA,
                                error201403TotalAglom$estimacion,
                                error201403TotalAglom$CVpp), #acá use la función definida arriba para alla el CV
              # la funcion devuelve el CV que corresponde al valor de referencia más cercano a la estimación realizada
              CVpptotal = closest(totalPoblacion,
                                  error201403TotalAglom$estimacion,
                                  error201403TotalAglom$CVpp), #idem anterior
              CVtasaActividad = round(sqrt((CVppPEA)^2+(CVpptotal)^2),2), #calculo el CV de la tasa como la
              # raiz cuadrada de a suma de los cuadrados de los coeficientes de variacion de las estimaciones que componen la tasa
              # según el manual de error de la EPH.
              DStasaActividad = round(tasaActividad*CVtasaActividad/100,2) 
              #calculo la DS de la tasa como el producto entre la tasa de actividad y su CV, redondeo a 2 digits
              )

#para el resto de los casos se repite el procedimiento
varon16a24 <- eph202001 %>% 
    filter(CH04 == 01,CH06 >= 16 & CH06<= 24) %>% 
    summarise(genero = "Varon",
        edad = "16 a 24",
              PEA = sum(PONDERA[ESTADO == 1 | ESTADO == 2]),
              totalPoblacion = sum(PONDERA),
              tasaActividad = round(PEA/totalPoblacion*100,2),
              CVppPEA = closest(PEA,
                                error201403TotalAglom$estimacion,
                                error201403TotalAglom$CVpp),
              CVpptotal = closest(totalPoblacion,
                                  error201403TotalAglom$estimacion,
                                  error201403TotalAglom$CVpp),
              CVtasaActividad = round(sqrt((CVppPEA)^2+(CVpptotal)^2),2),
              DStasaActividad = round(tasaActividad*CVtasaActividad/100,2)) 

grupo66ymas <- eph202001 %>% 
    filter(CH06 >= 66) %>% 
    summarise(genero = "Varon + Mujer" ,
              edad = "66 y más",
              PEA = sum(PONDERA[ESTADO == 1 | ESTADO == 2]),
              totalPoblacion = sum(PONDERA),
              tasaActividad = round(PEA/totalPoblacion*100,2),
              CVppPEA = closest(PEA,
                                error201403TotalAglom$estimacion,
                                error201403TotalAglom$CVpp),
              CVpptotal = closest(totalPoblacion,
                                  error201403TotalAglom$estimacion,
                                  error201403TotalAglom$CVpp),
              CVtasaActividad = round(sqrt((CVppPEA)^2+(CVpptotal)^2),2),
              DStasaActividad = round(tasaActividad*CVtasaActividad/100,2)) 

#esta tabla y la siguiente es para desempleo
varon18a30 <- eph202001 %>% 
    filter(CH06 >= 18 & CH06<=30, CH04 == 01) %>% 
    summarise(genero = "Varon",
              edad = "18 a 30",
              desempleo = sum(PONDERA[ESTADO == 2]),
              PEA = sum(PONDERA[ESTADO == 1 | ESTADO == 2]),
              tasaDesempleo = round(desempleo/PEA*100,2),
              CVppdesempleo = closest(desempleo,
                                  error201403TotalAglom$estimacion,
                                  error201403TotalAglom$CVpp),
              CVppPEA = closest(PEA,
                                error201403TotalAglom$estimacion,
                                error201403TotalAglom$CVpp),
              CVtasaDesempleo = round(sqrt((CVppPEA)^2+(CVppdesempleo)^2),2),
              DStasaDesempleo = round(tasaDesempleo*CVtasaDesempleo/100,2)) 

mujer25a40 <- eph202001 %>% 
    filter(CH06 >= 25 & CH06<=40, CH04 == 02) %>% 
    summarise(genero ="Mujer", edad = "25 a 40",
              desempleo = sum(PONDERA[ESTADO == 2]),
              PEA = sum(PONDERA[ESTADO == 1 | ESTADO == 2]),
              tasaDesempleo = round(desempleo/PEA*100,2),
              CVppdesempleo = closest(desempleo,
                                      error201403TotalAglom$estimacion,
                                      error201403TotalAglom$CVpp),
              CVppPEA = closest(PEA,
                                error201403TotalAglom$estimacion,
                                error201403TotalAglom$CVpp),
              CVtasaDesempleo = round(sqrt((CVppPEA)^2+(CVppdesempleo)^2),2),
              DStasaDesempleo = round(tasaDesempleo*CVtasaDesempleo/100,2)) 

#reuno en una sola tabla todo lo calculado para actividad
tablaResumenActividad <- bind_rows(grupo25a65, varon16a24, grupo66ymas)

tablaResumenActividad

#a partir de esa tabla calculo los limites inferio y superior para un 90% de confianza
tablaConfianzaActividad <- tablaResumenActividad %>% 
    select(genero, edad, DStasaActividad,tasaActividad) %>% 
    mutate(
        LiTasaActividad = round(tasaActividad - DStasaActividad*1.64,2),
        LsTasaActividad = round(tasaActividad + DStasaActividad*1.64,2)
    )

tablaConfianzaActividad

#hago lo mismo para desempleo
tablaResumenDesempleo <-  bind_rows(varon18a30, mujer25a40)
tablaConfianzaDesempleo <- tablaResumenDesempleo %>% 
    select(genero, edad, DStasaDesempleo,tasaDesempleo) %>% 
    mutate(
        LiTasaDesempleo = round(tasaDesempleo - DStasaDesempleo *1.64,2),
        LsTasaDesempleo = round(tasaDesempleo + DStasaDesempleo *1.64,2)
    )

#guardo las tablas como excel

write_excel_csv(tablaResumenActividad, "tablaActividad1.xls")
write_excel_csv(tablaConfianzaActividad, "tablaActividad2.xls")
write_excel_csv(tablaResumenDesempleo, "tablaDesempleo1.xls")
write_excel_csv(tablaConfianzaDesempleo, "tablaDesempleo2.xls")
